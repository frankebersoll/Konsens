[<WebSharper.JavaScript>]
module Konsens.Web.Update

open WebSharper
open WebSharper.Mvu
open Konsens.Web.Model

[<NamedUnionCases "type">]
type NeuMessage =
    | AddVorschlag
    | RemoveVorschlag of string
    | SetTitel of string
    | SetNeuerVorschlag of string
    | Anlegen

[<NamedUnionCases "type">]
type Message =
    | Goto of page: Page
    | NeuMessage of neu: NeuMessage
    | Error of error: string

module Commands =

    open Konsens.Domain.Model
    open Konsens.Web.Server

    let Anlegen (state: NeuState) (dispatch: Dispatch<Message>) = 
        async {
            let! (AbstimmungId id) = Neu state.Titel state.Vorschläge
            Page.Abstimmung id |> Goto |> dispatch
        }

let Goto (page: Page) (state: State) : State =
    let initPageState state = { state with PageState = PageState.Init state.Page }

    match page with
    | Home -> { state with Page = Home }
    | Neu -> { state with Page = Neu }
    | Abstimmung id -> { state with Page = Abstimmung id }

    |> initPageState

let private UpdateNeu (message: NeuMessage) (pageState: NeuState) state =
    let update s = s |> State.UpdateNeuState state |> Action.SetModel

    match message with
    | AddVorschlag -> pageState |> NeuState.AddVorschlag |> update
    | RemoveVorschlag vorschlag -> pageState |> NeuState.RemoveVorschlag vorschlag |> update
    | SetTitel titel -> pageState |> NeuState.SetTitel titel |> update
    | SetNeuerVorschlag vorschlag -> pageState |> NeuState.SetNeuerVorschlag vorschlag |> update
    | Anlegen -> Commands.Anlegen pageState |> CommandAsync

let UpdateApp (message: Message) (state: State) : Action<Message, State> =
    match (message, state.PageState) with
    | Goto page, _ ->
        UpdateModel (Goto page)
    | NeuMessage message, NeuState pageState -> 
        UpdateNeu message pageState state
    | Error error, _ -> 
        SetModel { state with Error = Some error }
    | _ -> 
        SetModel { state with Error = Some "Ungültiger Zustand" }



