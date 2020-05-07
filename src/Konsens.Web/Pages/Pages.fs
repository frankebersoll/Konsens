namespace Konsens.Web.Pages

[<WebSharper.JavaScript>]
module Update =

    open WebSharper
    open WebSharper.Mvu.Pages.Update
    open Konsens.Web.Shared
    open Konsens.Web.Routing

    [<NamedUnionCasesAttribute "type">]
    type CommonMessage =
        | Goto of Page
        | Error of string
        | ResetFocus
        | ResetError

    type CommonRequest =
        | Init
        | Push of PushMessage
        | RecoverFromError
        | ResetFocus

    type Request<'T> = Request<CommonRequest,'T>

    [<RequireQualifiedAccess>]
    module Out =

        type Message<'PageMsg> = Out.Message<CommonMessage,'PageMsg>
        type Dispatch<'PageMsg> = Out.Dispatch<CommonMessage,'PageMsg>

        let Goto page = CommonMessage.Goto page |> Out.CommonMessage
        let Error s = CommonMessage.Error s |> Out.CommonMessage
        let ResetFocus() = CommonMessage.ResetFocus |> Out.CommonMessage
        let ResetError() = CommonMessage.ResetError |> Out.CommonMessage
    
[<WebSharper.JavaScript>]
module View =
    
    open WebSharper.UI
    open WebSharper.UI.Client
    open Konsens.Web.Client
    open Update

    type DefaultLayout = { Icon: Doc; Subtitle: View<string option> }

    type Layout =
    | DefaultLayout of DefaultLayout
    | Fullscreen

    type LayoutResult = { Doc: Doc; Layout: Layout }

    let DefaultLayoutWithIcon icon = { Icon = icon; Subtitle = View.Const None }
    let WithSubtitle subtitle layout = { layout with Subtitle = subtitle }

    let WithLayout layout doc = { Doc = doc; Layout = layout }

    [<AutoOpen>]
    module Html =
    
        let DisableWhenFalse (isValid: View<bool>) =
            Attr.Prop "disabled" (not isValid.V)
        
        let IsLoading (isLoading: View<bool>) =
            Attr.DynamicClassPred "is-loading" isLoading

    [<RequireQualifiedAccess>]
    module Page =

        type Context<'PageModel,'PageMsg> = private {
            PageState : View<'PageModel>
            IsReady : View<bool>
            Dispatch : Out.Dispatch<'PageMsg>
        } with
            member this.Focus<'T when 'T : equality> f (value : 'T) =
                let shouldFocus = V ((f this.PageState.V) = Some value && this.IsReady.V)
                on.viewUpdate shouldFocus (fun el x -> 
                    if x then 
                        Focus el; Out.ResetFocus() |> this.Dispatch)

        let Context pageState isReady dispatch = {
            PageState = pageState
            IsReady = isReady
            Dispatch = dispatch
        }

