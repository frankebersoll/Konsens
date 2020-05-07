[<WebSharper.JavaScript>]
module Konsens.Web.Pages.Neu

open WebSharper
open Konsens.Utils
open Konsens.Web

module Model =

    [<NamedUnionCases "type">]
    type NeuFocus =
        | TitelFocus
        | VorschlagFocus

    type NeuState = 
        { 
            Titel: string
            Vorschläge: string list
            NeuerVorschlag: string
            CanAdd: bool
            IsValid: bool
            Focus: NeuFocus option
            IsSubmitting: bool
        }

    module NeuState =

        type Action = NoAction | Submit

        let Init = { 
            Titel = ""
            Vorschläge = []
            NeuerVorschlag = ""
            CanAdd = false
            IsValid = false
            Focus = Some TitelFocus
            IsSubmitting = false 
        }

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

        let SetFocus focus (state: NeuState) = { state with Focus = focus }

        let SetNeuerVorschlag vorschlag (state: NeuState) =
            { state with NeuerVorschlag = vorschlag }
            |> Validate

        let Umsortieren oldIndex newIndex (state: NeuState) =
            { state with Vorschläge = state.Vorschläge |> List.move oldIndex newIndex }

        let Submit (state: NeuState) =
            if state.IsValid then
                ({ state with IsSubmitting = true }, Submit)
            else
                (state, NoAction)

        let Recover (state: NeuState) = { state with IsSubmitting = false }

open WebSharper.Mvu
open WebSharper.Mvu.Pages

open Routing

module Update =

    open Konsens.Domain

    open Model
    open Update

    [<NamedUnionCases "type">]
    type NeuMessage =
        | AddVorschlag
        | RemoveVorschlag of string
        | SetTitel of string
        | SetNeuerVorschlag of string
        | SetFocus of NeuFocus
        | Umsortieren of int * int
        | Anlegen

    type Dispatch = Out.Dispatch<NeuMessage>
    
    let UpdateNeu (message: Request<NeuMessage>) (pageState: NeuState) =
    
        let anlegen (dispatch: Out.Dispatch<NeuMessage>) =
            async {
                let! (AbstimmungId id) = Server.Neu pageState.Titel pageState.Vorschläge
                Page.Abstimmung id |> Out.Goto |> dispatch
            }

        match message with
        | CommonRequest action ->
            match action with
            | Init -> DoNothing
            | Push _ -> DoNothing
            | RecoverFromError -> NeuState.Recover |> UpdateModel
            | ResetFocus -> NeuState.ResetFocus |> UpdateModel
        | PageMessage message ->
            match message with
            | AddVorschlag -> NeuState.AddVorschlag |> UpdateModel
            | RemoveVorschlag vorschlag -> NeuState.RemoveVorschlag vorschlag |> UpdateModel
            | SetTitel titel -> NeuState.SetTitel titel |> UpdateModel
            | SetNeuerVorschlag vorschlag -> NeuState.SetNeuerVorschlag vorschlag |> UpdateModel
            | SetFocus focus -> NeuState.SetFocus (Some focus) |> UpdateModel
            | Umsortieren (oldIndex, newIndex) -> NeuState.Umsortieren oldIndex newIndex |> UpdateModel
            | Anlegen -> NeuState.Submit |> UpdateX pageState (function
                | NeuState.Submit -> anlegen |> CommandAsync
                | NeuState.NoAction -> DoNothing)

module View =

    open WebSharper.UI

    open Client
    open Model
    open Update
    open View

    type Template = Templating.Template<"Pages/Neu/Neu.html", Templating.ClientLoad.FromDocument>

    type Context = Page.Context<NeuState, NeuMessage>

    let VorschlagRow dispatch (titel: string) =
        Template.Vorschlag()
            .Titel(titel)
            .DeleteOnClick(fun _ -> RemoveVorschlag titel |> dispatch)
            .Doc()

    let Page = 
        Page.Build
        |> Page.WithAttribute (Attr.Class "neu-page")
        |> Page.UsesTransition
        |> Page.RenderSingle (fun (dispatch: Dispatch) page (ctx: Context) ->
        
            let dispatch = Out.PageMessage >> dispatch

            let hinzufügen () = AddVorschlag |> dispatch
            let anlegen () = Anlegen |> dispatch
        
            let focus = ctx.Focus (fun neuState -> neuState.Focus)
            let sortable = Lib.Sortable.Create (Umsortieren >> dispatch)
            let hasTitel = V (not page.V.Titel.IsNullOrWhitespace)
            let hasNeuerVorschlag = V (not page.V.NeuerVorschlag.IsNullOrWhitespace)
            
            Template
                .NeueAbstimmung()

                .Titel(V page.V.Titel, SetTitel >> dispatch)
                .TitelAttr(
                    focus TitelFocus,
                    onEnterIf hasTitel (fun () -> SetFocus VorschlagFocus |> dispatch)
                )

                .Vorschlaege(V page.V.Vorschläge |> Doc.BindSeqCached (VorschlagRow dispatch))
                .VorschlaegeAttr(Attr.OnAfterRender sortable)

                .Vorschlag(V page.V.NeuerVorschlag, SetNeuerVorschlag >> dispatch)
                .VorschlagAttr(
                    focus VorschlagFocus,
                    onEnterIfElse hasNeuerVorschlag hinzufügen anlegen
                )

                .AddAttr(
                    V page.V.CanAdd |> DisableWhenFalse,
                    on.click (fun _ _ -> hinzufügen())
                )

                .AbbrechenLink(Router.Link Page.Home)

                .AnlegenAttr(
                    V page.V.IsValid |> DisableWhenFalse,
                    V page.V.IsSubmitting |> IsLoading,
                    V page.V.Focus |> onViewIf (Some VorschlagFocus) (fun e -> e.ScrollIntoView())
                    )

                .FormOnSubmit(fun e -> e.Event.PreventDefault(); anlegen())
                .Doc()
                
                |> WithLayout (DefaultLayoutWithIcon (Template.NeuIcon().Doc()) |> DefaultLayout)
        )