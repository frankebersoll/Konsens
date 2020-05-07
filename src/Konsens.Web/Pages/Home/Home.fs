[<WebSharper.JavaScript>]
module Konsens.Web.Pages.Home

open WebSharper
open Konsens.Web

module Model =

    open Konsens.Application.Model

    [<NamedUnionCases "type">]
    type CodeInput =
    | NoCodeInput
    | EnteringCode of string
    | CheckingCode of string
    | CodeInvalid

    type HomeState = 
        {
            Code: CodeInput
        }

    module HomeState =

        type Action =
        | NoAction
        | CheckCode of string

        let Init = 
            { 
                Code = NoCodeInput
            }

        let EnterCode state : HomeState = { state with Code = EnteringCode "" }

        let LeaveCodeEntry state : HomeState = { state with Code = NoCodeInput }

        let SetCode (code: string) (state : HomeState) = 
            match state.Code with
            | EnteringCode _ ->
                let trimLength = min code.Length CodeLength
                let code = code.ToUpper().Substring(0, trimLength)
                let isComplete = code.Length = CodeLength
                (
                    { state with Code = if isComplete then CheckingCode code else EnteringCode code },
                    if isComplete then CheckCode code else NoAction
                )
            | _ -> (state, NoAction)

        let SetCodeInvalid state : HomeState = { state with Code = CodeInvalid }

open WebSharper.Mvu
open WebSharper.Mvu.Pages

open Routing

module Update =

    open Model
    open Update

    [<NamedUnionCases "type">]
    type HomeMessage =
        | EnterCode
        | LeaveCodeEntry
        | SetCode of string
        | CodeNotFound

    type Dispatch = Out.Dispatch<HomeMessage>

    let UpdateHome (message: Request<HomeMessage>) (pageState: HomeState) =

        let checkCode (code: string) (dispatch: Dispatch) =
            async {
                let! result = Server.GetAbstimmungIdByCode code
                match result with
                | Some id -> Page.Abstimmung id |> Out.Goto |> dispatch
                | None -> CodeNotFound |> Out.PageMessage |> dispatch
            }

        match message with
        | CommonRequest action ->
            match action with
            | Init -> DoNothing
            | Push _ -> DoNothing
            | RecoverFromError -> DoNothing
            | ResetFocus -> DoNothing
        | PageMessage message ->
            match message with
            | EnterCode -> HomeState.EnterCode |> UpdateModel
            | LeaveCodeEntry -> HomeState.LeaveCodeEntry |> UpdateModel
            | SetCode code -> HomeState.SetCode code |> UpdateX pageState (function
                | HomeState.CheckCode c -> checkCode c |> CommandAsync
                | HomeState.NoAction -> DoNothing)
            | CodeNotFound ->
                (HomeState.SetCodeInvalid |> UpdateModel) +
                Action.DispatchAsync (fun () -> Out.PageMessage EnterCode) (Async.Sleep 1000)

module View =

    open WebSharper.UI

    open Client
    open Model
    open Update
    open View

    type Template = Templating.Template<"Pages/Home/Home.html", Templating.ClientLoad.FromDocument>

    type Context = Page.Context<HomeState, HomeMessage>

    let Page =
        Page.Build
        |> Page.WithAttribute (Attr.Class "home-page")
        |> Page.UsesTransition
        |> Page.RenderSingle (fun (dispatch: Dispatch) page (ctx: Context) ->
        
            let dispatch = Out.PageMessage >> dispatch

            let codeText = V (page.V.Code |> function
                | EnteringCode c -> c
                | CheckingCode c -> c
                | _ -> "")

            let showCodeEntry = V (page.V.Code |> function NoCodeInput -> false | _ -> true)
            let canEnter = V (page.V.Code |> function EnteringCode _ -> true | _ -> false)
            let isChecking = V (page.V.Code |> function CheckingCode _ -> true | _ -> false)
            let isInvalid = V (page.V.Code |> function CodeInvalid -> true | _ -> false)

            Template.Home()
                .Attr(Attr.OnAfterRender Lib.configureAnimations)
                .NeuLink(Router.Link Page.Neu)
                .CodeCtrlAttr(Attr.DynamicClassPred "is-loading" isChecking)
                .Code(codeText, SetCode >> dispatch)
                .CodeAttr(
                    Attr.DynamicClassPred "is-danger" isInvalid,
                    canEnter |> DisableWhenFalse,
                    on.viewUpdate canEnter (fun el x -> if x then Focus el),
                    on.blurView canEnter (fun _ _ x -> if x then LeaveCodeEntry |> dispatch)
                )
                .CodeContainerAttr(Attr.DynamicClassPred "code-container--entry" showCodeEntry)
                .CodeButtonOnClick(fun _ -> EnterCode |> dispatch)
                .Doc()

            |> WithLayout Fullscreen)
