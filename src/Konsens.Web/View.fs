[<WebSharper.JavaScript>]
module Konsens.Web.View

open WebSharper.UI
open WebSharper.JavaScript
open WebSharper.Mvu
open WebSharper.Mvu.Pages.View

open Konsens.Utils
open Konsens.Web
open Konsens.Web.Pages

open Routing
open Client
open Model
open Update
open View

[<AutoOpen>]
module Templating =

    type MainTemplate = Templating.Template<"Pages/index.html", Templating.ClientLoad.FromDocument>

    let private Error (message: string) = 
        MainTemplate
            .Error()
            .Message(message)
            .Elt()
            .OnAfterRender(fun e -> Focus e)
            :> Doc

    let Layout layout (state: View<CommonState>) (dispatch: Out.Dispatch<_>) (content: Doc) =

        let content =
            match layout with
            | View.DefaultLayout l -> 
                MainTemplate.DefaultLayout()
                    .Icon(l.Icon)
                    .Title(state.V.Title)
                    .Subtitle(V (l.Subtitle.V |?? ""))
                    .SubtitleAttr(
                        Attr.ClassPred "is-hidden" (l.Subtitle.V |> Option.isNone)
                    )
                    .Content(content)
                    .Doc()
            | View.Fullscreen -> MainTemplate.FullscreenLayout().Content(content).Doc()

        let error = V ( match state.V.Error with Some e -> Error e | None -> Doc.Empty )
        let isActive = V ( state.V.Error |> Option.isSome )
        let reset () = Out.ResetError() |> dispatch

        MainTemplate
            .LayoutContainer()
            .Content(content)
            .ModalContent(error.V)
            .ModalAttr(
                Attr.DynamicClassPred "is-active" isActive,
                on.click (fun _ _ -> reset()),
                onEscape reset
            )
            .Doc()
  
let mapLayout sharedState dispatch (layoutResult: View.LayoutResult) =
    Templating.Layout layoutResult.Layout sharedState dispatch layoutResult.Doc

let WithPageState page definition =
    WithPageState 
        CommonState.Init
        Page.Context
        mapLayout
        page 
        definition

let PageHome = WithPageState Pages.Home.View.Page HomeDefinition
let PageNeu = WithPageState Pages.Neu.View.Page NeuDefinition
let PageAbstimmung = WithPageState Pages.Abstimmung.View.Page AbstimmungDefinition

let Page state =
    match state.Page with
    | Page.Home -> PageHome ()
    | Page.Neu -> PageNeu ()
    | Page.Abstimmung id -> PageAbstimmung id

let HandleUpdate (state: View<State>) =
    [
        V (state.V.Shared.IsLoading) |> View.MapCached (fun isLoading -> 
            if isLoading then 
                Lib.Progress.Start() 
            else 
                Lib.Progress.Done()
        )

        V state.V.Shared.Title |> View.MapCached (fun title ->
            JS.Document.Title <- "Konsens - " + title
        )
    ]
