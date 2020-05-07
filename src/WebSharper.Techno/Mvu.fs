namespace WebSharper.Mvu

#nowarn "40" // let rec container

open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
type App<'Message, 'Model, 'Rendered> =
    internal {
        Init : Dispatch<'Message> -> unit
        Var : Var<'Model>
        View : View<'Model>
        Update : Dispatch<'Message> -> 'Message -> 'Model -> option<'Model>
        Render : Dispatch<'Message> -> View<'Model> -> 'Rendered
    }

[<AutoOpen; JavaScript>]
module Action =
    /// Run the given asynchronous job then dispatch a message based on its result.
    let DispatchAsync (toMessage: 'T -> 'Message) (action: Async<'T>) : Action<'Message, 'Model> =
        CommandAsync (fun dispatch -> async {
            let! res = action
            dispatch (toMessage res)
        })

[<JavaScript>]
type RenderInfo<'Model> = 
    {
        Model: View<'Model>
        IsReady: View<bool>
    }

[<Require(typeof<Resources.PagerCss>)>]
[<JavaScript>]
type Page<'Message, 'Model> =
    internal {
        Render: Pager<'Message, 'Model> -> Dispatch<'Message> -> View<'Model> -> Elt * Var<bool>
        KeepInDom: bool
        UsesTransition: bool
    }

    static member Reactive
        (
            key: 'EndPointArgs -> 'K,
            render: 'K -> Dispatch<'Message> -> RenderInfo<'Model> -> Doc,
            ?attrs: seq<Attr>,
            ?keepInDom: bool,
            ?usesTransition: bool
        ) =
        let dic = Dictionary()
        let mutable cacheVersion = 0
        let clearCacheIfNeeded () =
            let currentVersion = LiveReload.Client.GetVersion()
            if cacheVersion <> currentVersion then
                dic.Clear()
                cacheVersion <- currentVersion
        let getOrRender (route: 'EndPointArgs) (pager: Pager<'Message, 'Model>) (dispatch: Dispatch<'Message>) (model: View<'Model>) =
            let k = key route
            clearCacheIfNeeded()
            match dic.TryGetValue k with
            | true, result -> result
            | false, _ ->
                let isActive = Var.Create false
                let info = { Model = model; IsReady = isActive.View }
                let rec doc =
                    Elt.div [
                        attr.``class`` "ws-page"
                        (match attrs with Some attrs -> Attr.Concat attrs | None -> Attr.Empty)
                        on.transitionEnd (fun _ _ ->
                            if pager.RemoveIfNeeded doc then Var.Set isActive true)
                    ] [render k dispatch info]
                let result = (doc, isActive)
                dic.[k] <- result
                result
        fun ep ->
            {
                Render = getOrRender ep
                KeepInDom = defaultArg keepInDom false
                UsesTransition = defaultArg usesTransition false
            } : Page<'Message, 'Model>

    static member Create(render, ?attrs, ?keepInDom, ?usesTransition) : 'EndPointArgs -> _ =
        Page<'Message, 'Model>.Reactive(id, render, ?attrs = attrs, ?keepInDom = keepInDom, ?usesTransition = usesTransition)

    static member Single(render, ?attrs, ?keepInDom, ?usesTransition) =
        Page<'Message, 'Model>.Reactive((fun () -> ()), (fun () -> render), ?attrs = attrs, ?keepInDom = keepInDom, ?usesTransition = usesTransition)

and [<JavaScript>] internal Pager<'Message, 'Model>(render: 'Model -> Page<'Message, 'Model>, dispatch: Dispatch<'Message>, model: View<'Model>) as this =
    let mutable active = None : (Elt * Var<bool> * bool) option
    let mutable dead = None : Elt option

    let remove (toRemove: Elt) (container: EltUpdater) =
        if container.Dom.Contains toRemove.Dom then
            container.Dom.RemoveChild toRemove.Dom |> ignore
            container.RemoveUpdated toRemove

    let modelWithLiveReload = View.Map2 (fun m _ -> m) model LiveReload.Client.View

    let rec container : EltUpdater =
        let onViewUpdate (el: Dom.Element) r =
            let page = render r
            let (elt, isActive) = page.Render this dispatch model
            let domElt = elt.Dom
            domElt.RemoveAttribute("aria-hidden")
            match active with
            | Some (toRemove, isActive, usesTransition) when toRemove !==. elt ->
                Var.Set isActive false
                if not usesTransition then
                    if not page.UsesTransition then container |> remove toRemove
                    else dead <- Some toRemove
                else
                    toRemove.Dom.SetAttribute("aria-hidden", "true")
            | _ -> ()
            if not (el.Contains domElt) then
                domElt.SetAttribute("aria-hidden", "true")
                el.AppendChild domElt |> ignore
                container.AddUpdated elt
                JS.RequestAnimationFrame (fun _ -> domElt.RemoveAttribute("aria-hidden")) |> ignore
            active <- if page.KeepInDom then None else Some (elt, isActive, page.UsesTransition)
            if not page.UsesTransition then Var.Set isActive true

        let elt =
            Elt.div [
                attr.``class`` "ws-page-container"
                on.viewUpdate modelWithLiveReload onViewUpdate
            ] []

        elt.ToUpdater()

    member __.RemoveIfNeeded(elt: Elt): bool =
        match active with
        | Some (toRemove, _, _) when toRemove ===. elt ->
            match dead with
            | Some elt -> container |> remove elt; dead <- None; true
            | None -> true
        | Some _ -> container |> remove elt; false
        | None -> false

    member __.Doc = container :> Doc

    
[<JavaScript>]
module App =

    let private create initModel update render =
        let var = Var.Create initModel
        {
            Init = ignore
            Var = var
            View = var.View
            Update = update
            Render = render
        }

    let CreateSimple<'Message, 'Model, 'Rendered>
            (initModel: 'Model)
            (update: 'Message -> 'Model -> 'Model)
            (render: Dispatch<'Message> -> View<'Model> -> 'Rendered) =
        let update _ msg mdl =
            Some (update msg mdl)
        create initModel update render

    let rec private applyAction dispatch oldModel = function
        | DoNothing -> None
        | SetModel mdl -> Some mdl
        | UpdateModel f -> Some (f oldModel)
        | Command f -> f dispatch; None
        | CommandAsync f -> Async.Start (f dispatch); None
        | CombinedAction actions ->
            (None, actions)
            ||> List.fold (fun newModel action ->
                applyAction dispatch (defaultArg newModel oldModel) action
                |> Option.orElse newModel
            )

    let Create<'Message, 'Model, 'Rendered>
            (initModel: 'Model)
            (update: 'Message -> 'Model -> Action<'Message, 'Model>)
            (render: Dispatch<'Message> -> View<'Model> -> 'Rendered) =
        let update dispatch msg mdl =
            update msg mdl |> applyAction dispatch mdl
        create initModel update render

    let CreatePaged<'Message, 'Model>
            (initModel: 'Model)
            (update: 'Message -> 'Model -> Action<'Message, 'Model>)
            (render: 'Model -> Page<'Message, 'Model>) =
        let render (dispatch: Dispatch<'Message>) (view: View<'Model>) =
            Pager<'Message, 'Model>(render, dispatch, view).Doc
        Create initModel update render

    let CreateSimplePaged<'Message, 'Model>
            (initModel: 'Model)
            (update: 'Message -> 'Model -> 'Model)
            (render: 'Model -> Page<'Message, 'Model>) =
        let update msg mdl =
            SetModel (update msg mdl)
        CreatePaged initModel update render

    let WithHtml5Routing<'Route, 'Message, 'Model, 'Rendered when 'Route : equality>
            (router: WebSharper.Sitelets.Router<'Route>)
            (getRoute: 'Model -> 'Route)
            (update: Dispatch<'Message> -> 'Route -> unit)
            (app: App<'Message, 'Model, 'Rendered>) =
        { app with
            Init = fun dispatch ->
                let view = app.Var.View |> View.Map getRoute
                let var = Var.Make view (update dispatch)
                app.Init dispatch
                let defaultRoute = getRoute app.Var.Value
                Router.InstallInto var defaultRoute router }

    let private withLocalStorage
            (serializer: Serializer<'Model>)
            (key: string)
            (app: App<_, 'Model, _>) =
        let init dispatch =
            match JS.Window.LocalStorage.GetItem(key) with
            | null -> ()
            | v -> 
                try app.Var.Set <| serializer.Decode (JSON.Parse v)
                with exn ->
                    Console.Error("Error deserializing state from local storage", exn)
            app.Init dispatch
        let view =
            app.View.Map(fun v ->
                JS.Window.LocalStorage.SetItem(key, JSON.Stringify (serializer.Encode v))
                v
            )
        { app with View = view; Init = init }

    [<Inline>]
    let WithLocalStorage key (app: App<_, 'Model, _>) =
        withLocalStorage Serializer.Typed<'Model> key app

    let WithInitAction (action: Action<'Message, 'Model>) (app: App<'Message, 'Model, _>) =
        let init dispatch =
            app.Init dispatch
            applyAction dispatch app.Var.Value action
            |> Option.iter app.Var.Set
        { app with Init = init }

    let WithInitMessage (message: 'Message) (app: App<'Message, 'Model, 'Rendered>) =
        WithInitAction (Command (fun dispatch -> dispatch message)) app

    let Run (app: App<_, _, _>) =
        let rec dispatch msg = app.Var.UpdateMaybe (app.Update dispatch msg)
        app.Init dispatch
        app.Render dispatch app.View

    let WithLog
            (log: 'Message -> 'Model -> unit)
            (app: App<'Message, 'Model, _>) =
        let update dispatch msg model =
            let newModel = app.Update dispatch msg model
            log msg (defaultArg newModel model)
            newModel
        { app with Update = update }

    let WithUpdateHandler (f: View<'Model> -> View<unit> list) (app: App<'Message, 'Model, 'Rendered>) =
        let sequence = app.View |> f |> View.Sequence
        let view = app.View |> View.Map2 (fun _ model -> model) sequence
        { app with View = view }

    let private withRemoteDev
            (msgSerializer: Serializer<'Message>)
            (modelSerializer: Serializer<'Model>)
            (options: RemoteDev.Options)
            (app: App<'Message, 'Model, _>) =
        let rdev = RemoteDev.ConnectViaExtension(options)
        // Not sure why this is necessary :/
        let decode (m: obj) =
            match m with
            | :? string as s -> modelSerializer.Decode (JSON.Parse s)
            | m -> modelSerializer.Decode m
        rdev.subscribe(fun msg ->
            if msg.``type`` = RemoteDev.MsgTypes.Dispatch then
                match msg.payload.``type`` with
                | RemoteDev.PayloadTypes.JumpToAction
                | RemoteDev.PayloadTypes.JumpToState ->
                    let state = decode (RemoteDev.ExtractState msg)
                    app.Var.Set state
                | RemoteDev.PayloadTypes.ImportState ->
                    let state = msg.payload.nextLiftedState.computedStates |> Array.last
                    let state = decode state?state
                    app.Var.Set state
                    rdev.send(null, msg.payload.nextLiftedState)
                | _ -> ()
        )
        |> ignore
        let update dispatch msg model =
            let newModel = app.Update dispatch msg model
            match newModel with
            | Some newModel ->
                rdev.send(
                    msgSerializer.Encode msg,
                    modelSerializer.Encode newModel
                )
            | None -> ()
            newModel
        let init dispatch =
            app.Init dispatch
            app.View |> View.Get (fun st ->
                rdev.init(modelSerializer.Encode st)
            )
        { app with Init = init; Update = update }

    [<Inline>]
    let WithRemoteDev options (app: App<'Message, 'Model, _>) =
        withRemoteDev Serializer.Typed<'Message> Serializer.Typed<'Model> options app