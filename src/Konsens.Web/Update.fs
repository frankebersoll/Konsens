[<WebSharper.JavaScript>]
module Konsens.Web.Update

open WebSharper
open WebSharper.Mvu

open Routing
open Shared
open Model

open Pages.Update
open Pages.Home.Update
open Pages.Neu.Update
open Pages.Abstimmung.Update

[<NamedUnionCases "type">]
type Message =
    | Startup of Client.Configuration
    | CommonMessage of CommonMessage
    | HomeMessage of HomeMessage
    | NeuMessage of NeuMessage
    | AbstimmungMessage of AbstimmungMessage
    | PushMessage of PushMessage

let Define x =
    Define 
        (fun s -> s.PageState)
        (fun p s -> { s with PageState = p })
        (fun s -> s.Shared)
        CommonMessage
        x

let HomeDefinition = 
    Define HomeState HomeMessage UpdateHome 
        <| function HomeState s -> Some s | _ -> None

let NeuDefinition =
    Define NeuState NeuMessage UpdateNeu 
        <| function NeuState s -> Some s | _ -> None

let AbstimmungDefinition =
    Define AbstimmungState AbstimmungMessage UpdateAbstimmung 
        <| function AbstimmungState s -> Some s | _ -> None

let private RunPageAction action state =
    match state with
    | NoPageState -> DoNothing
    | HomeState s -> HomeDefinition.RunAction s action
    | NeuState s -> NeuDefinition.RunAction s action
    | AbstimmungState s -> AbstimmungDefinition.RunAction s action

let private Goto (page: Page) =
    let state = State.Goto page
    let initAction = state.PageState |> RunPageAction CommonRequest.Init
    (state |> SetModel) + initAction
             
let private Startup (configuration: Client.Configuration) (dispatch: Dispatch<Message>) =
    let push json = 
        let message = Json.Deserialize<PushMessage> json
        PushMessage message |> dispatch
    Client.Init configuration push

let private Push (message: PushMessage) = RunPageAction (CommonRequest.Push message)

let private RecoverFromError (page: Page) (pageState: PageState) =
    let resetError = State.SetError None |> UpdateModel
    let pageAction = pageState |> RunPageAction CommonRequest.RecoverFromError
    match pageAction with
        | DoNothing -> Goto page
        | _ -> resetError + pageAction

let private ResetFocus = RunPageAction CommonRequest.ResetFocus
let private UpdatePage definition = definition.UpdatePage
    
let private Update (message: Message) (state: State) : Action<Message, State> =
    match (message, state.PageState) with

    | Startup configuration, _ -> Command (Startup configuration)
    | PushMessage message, pageState -> Push message pageState

    | CommonMessage shared, pageState ->
        match (shared, pageState) with
        | CommonMessage.Goto page, _ -> Goto page
        | CommonMessage.Error error, _ -> State.SetError (Some error) |> UpdateModel
        | CommonMessage.ResetError, pageState -> RecoverFromError state.Page pageState
        | CommonMessage.ResetFocus, pageState -> ResetFocus pageState

    | HomeMessage message, HomeState pageState -> UpdatePage HomeDefinition message pageState
    | NeuMessage message, NeuState pageState -> UpdatePage NeuDefinition message pageState
    | AbstimmungMessage message, AbstimmungState pageState -> UpdatePage AbstimmungDefinition message pageState
    | NeuMessage _, _ -> DoNothing
    | HomeMessage _, _ -> DoNothing
    | AbstimmungMessage _, _ -> DoNothing
        
let UpdateApp (message: Message) (state: State) : Action<Message, State> =
    
    let setIsLoading action =
        action +
        UpdateModel (fun s -> { s with Shared = { s.Shared with IsLoading = State.IsLoading s }})

    Update message state 
        |> setIsLoading
        |> Remoting.HandleExceptions (Error >> CommonMessage) Client.ReloadFromServer
