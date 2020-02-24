[<WebSharper.JavaScript>]
module Konsens.Web.Model

open WebSharper
open Konsens.Domain.Model
open Konsens.Utils

type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/neu">] Neu
    | [<EndPoint "/abstimmung">] Abstimmung of System.Guid
    with
    static member ofAbstimmung (AbstimmungId id) = Abstimmung id

type NeuFocus =
    | TitelFocus
    | VorschlagFocus

type NeuState = { 
    Titel: string
    Vorschläge: string list
    NeuerVorschlag: string
    CanAdd: bool
    IsValid: bool
    Focus: NeuFocus option }

module NeuState =

    let Init = { 
        Titel = ""
        Vorschläge = []
        NeuerVorschlag = ""
        CanAdd = false
        IsValid = false
        Focus = Some TitelFocus }

    let Validate state =
        let isValid = not ( state.Vorschläge.IsEmpty || state.Titel.IsNullOrWhitespace )
        let containsVorschlag = state.Vorschläge |> List.contains state.NeuerVorschlag
        let canAdd = (not containsVorschlag) && (not state.NeuerVorschlag.IsNullOrWhitespace)
        { state with IsValid = isValid; CanAdd = canAdd }

    let ResetFocus (state: NeuState) = { state with Focus = None }

    let AddVorschlag (state: NeuState) = 
        if state.CanAdd then
            { state with 
                Vorschläge = state.Vorschläge @ [ state.NeuerVorschlag ]
                NeuerVorschlag = ""
                Focus = Some VorschlagFocus }
        else
            state
        |> Validate

    let RemoveVorschlag vorschlag (state: NeuState) = 
        { state with 
            Vorschläge = state.Vorschläge |> List.filter (fun x -> x <> vorschlag) 
            NeuerVorschlag = vorschlag
            Focus = Some VorschlagFocus }
        |> Validate

    let SetTitel titel (state: NeuState) = 
        { state with Titel = titel }
        |> Validate
        |> ResetFocus

    let SetNeuerVorschlag vorschlag (state: NeuState) =
        { state with NeuerVorschlag = vorschlag }
        |> Validate
        |> ResetFocus


type PageState =
    | NoPageState
    | NeuState of NeuState

module PageState =
    let Init = function
        | Home -> NoPageState
        | Neu -> NeuState NeuState.Init
        | Abstimmung _ -> NoPageState

type State =
    {
        Page: Page
        PageState: PageState
        Error: string option
    }

module State =

    let Init : State =
        let startPage = Home
        {
            Page = startPage
            PageState = PageState.Init startPage
            Error = None
        }

    let UpdateNeuState state pageState = { state with PageState = NeuState pageState }
