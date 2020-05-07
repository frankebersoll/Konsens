[<WebSharper.JavaScript>]
module Konsens.Web.Model

open WebSharper

open Konsens.Domain.Model
open Pages.Home.Model
open Pages.Neu.Model
open Pages.Abstimmung.Model

open Routing

[<NamedUnionCases "type">]
type PageState =
    | NoPageState
    | HomeState of HomeState
    | NeuState of NeuState
    | AbstimmungState of AbstimmungState

module PageState =
    let Init = function
        | Page.Home -> HomeState HomeState.Init
        | Page.Neu -> NeuState NeuState.Init
        | Page.Abstimmung id -> AbstimmungState (AbstimmungState.Create (AbstimmungId id))

type CommonState = {
    Title: string
    Error: string option
    IsLoading: bool
}

module CommonState =
    let Init = {   
        Title = ""
        Error = None
        IsLoading = false 
    }

type State =
    {
        Shared: CommonState
        Page: Page
        PageState: PageState
    }

module State =

    let GetTitle (page: Page) = 
        match page with
        | Page.Home -> "Home"
        | Page.Neu -> "Neue Abstimmung"
        | Page.Abstimmung _ -> "Abstimmung"

    let Goto page = {
        Page = page
        PageState = PageState.Init page
        Shared = {
            Title = GetTitle page
            Error = None
            IsLoading = false 
        }
    }

    let Init : State = Goto Page.Home

    let SetPageState pageState state = { state with PageState = pageState }

    let SetError error state = { state with Shared = { state.Shared with Error = error } }

    let IsLoading state = 
        if state.Shared.Error |> Option.isSome then false
        else
            match state.PageState with
            | NeuState neuState -> neuState.IsSubmitting
            | HomeState homeState -> 
                match homeState.Code with
                | CodeInput.CheckingCode _ -> true
                | _ -> false
            | _ -> false
