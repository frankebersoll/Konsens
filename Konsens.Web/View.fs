[<WebSharper.JavaScript>]
module Konsens.Web.View

open WebSharper
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html
open WebSharper.JavaScript
open WebSharper.Mvu
open Konsens.Web.Model
open Konsens.Web.Update
open Konsens.Web.Client

let Router = Sitelets.InferRouter.Router.Infer<Page> ()

[<AutoOpen>]
module private Utils =

    let disableWhenFalse (isValid: View<bool>) =
        let isInvalid = V (not isValid.V)
        attr.disabledDynPred (View.Const "") isInvalid

    let onFocus predicate (action: Dom.Element -> unit) view =
        on.viewUpdate view (fun e focus -> 
            match focus with
            | Some x when x = predicate -> action e
            | _ -> ()
        )

    type KeyboardTemplateEvent<'T> = Templating.Runtime.Server.TemplateEvent<'T, Dom.KeyboardEvent>

    let onKey key f (e: KeyboardTemplateEvent<'T>) = 
        match e.Event with
        | Keyboard k when k = key -> e.Event.PreventDefault(); f()
        | _ -> ()

    let autofocus predicate = onFocus predicate (fun e -> (e :?> HTMLInputElement).Focus())

    let GetPageState f (state: View<State>) = state.Map (fun s -> f s.PageState)


[<AutoOpen>]
module private Templating =

    type Template = Templating.Template<"wwwroot/index.html", Templating.ClientLoad.FromDocument>

    type Layout =
    | Default
    | Fullscreen

    let Layout layout title (content: Doc) =
        JS.Document.Title <- sprintf "Konsens - %s" title
        match layout with
        | Default -> Template.DefaultLayout().Title(title).Content(content).Doc()
        | Fullscreen -> Template.FullscreenLayout().Content(content).Doc()

module Home =

    [<Inline("configureAnimations();")>]
    let animate () = ()
    let Page = Page.Single(attrs = [Attr.Class "home-page"], usesTransition = true, render = fun (dispatch: Dispatch<Message>) (state: View<State>) -> 
        Template.Home()
            .AnimateCallback(<@ animate @>)
            .NeuLink(Router.Link Neu)
            .Doc()
        |> Layout Fullscreen "Home")

module Neu =

    let VorschlagRow dispatch (titel: string) =
        Template.Vorschlag()
            .Titel(titel)
            .DeleteOnClick(fun _ -> RemoveVorschlag titel |> dispatch)
            .Doc()

    let Page = Page.Single(attrs = [Attr.Class "entry-page"], usesTransition = true, render = fun (dispatch: Dispatch<Message>) (state: View<State>) -> 

        let dispatch = NeuMessage >> dispatch
        let state = state |> GetPageState (function NeuState s -> s | _ -> NeuState.Init)
        let hinzufügen () = AddVorschlag |> dispatch
        let focus = V (state.V.Focus)

        Template.NeueAbstimmung()
                .Titel(V state.V.Titel, SetTitel >> dispatch)
                .TitelAttr(focus |> autofocus TitelFocus)
                .Vorschlaege((V state.V.Vorschläge).DocSeqCached(VorschlagRow dispatch))
                .Vorschlag(V state.V.NeuerVorschlag, SetNeuerVorschlag >> dispatch)
                .VorschlagAttr(focus |> autofocus VorschlagFocus)
                .VorschlagOnKeyDown(onKey EnterKey hinzufügen)
                .AddOnClick(fun _ -> hinzufügen())
                .AddAttr(V state.V.CanAdd |> disableWhenFalse)
                .AbbrechenLink(Router.Link Home)
                .SubmitAttr(
                    V state.V.IsValid |> disableWhenFalse,
                    focus |> onFocus VorschlagFocus (fun e -> e.ScrollIntoView())
                 )
                .FormOnSubmit(fun e -> e.Event.PreventDefault(); Anlegen |> dispatch)
                .Elt()

        |> Layout Default "Neue Abstimmung")

module Abstimmung =
    let Page = Page.Create(fun (id: System.Guid) (dispatch: Dispatch<Message>) (state: View<State>) -> Template.Abstimmung().Doc())

let Page state =
    match state.Page with
    | Home -> Home.Page ()
    | Neu -> Neu.Page ()
    | Abstimmung id -> Abstimmung.Page id
