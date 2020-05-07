[<WebSharper.JavaScript>]
module Konsens.Web.Client

open WebSharper
open WebSharper.JavaScript

open Konsens.Utils

[<AutoOpen>]
module Extensions =
    type Dom.Element with
        [<Inline("$this.scrollIntoView()")>]
        member this.ScrollIntoView () = ()

    type HTMLInputElement with
        member this.Clear () =
            this.Value <- "" 
            this.DispatchEvent(Dom.Event("change")) |> ignore

    type WebSharper.JavaScript.Location with
        [<Inline("$this.reload($noCache)")>]
        member this.Reload(noCache: bool) = ()

    type Dom.EventTarget with
        member this.AddKeyDownHandler f = 
            this.AddEventListener ("keydown", fun (e: Dom.Event) -> f (e :?> Dom.KeyboardEvent))

[<AutoOpen>]
module Html =
    open WebSharper.UI
    open WebSharper.UI.Client

    type KeyboardKey = EnterKey | EscapeKey | UnknownKey

    let (|Keyboard|) (e: Dom.KeyboardEvent) = 
        match e.KeyCode with
        | 13 -> EnterKey
        | 27 -> EscapeKey
        | _ ->  UnknownKey

    let getKey = (|Keyboard|)

    let matchKey (f : KeyboardKey -> bool) onEvent : Attr =
        onEvent (fun (_: Dom.Element) (e: Dom.KeyboardEvent) -> 
            if f (getKey e) then e.PreventDefault())

    let matchKeyValue (f : KeyboardKey * 'T -> bool) onEvent : Attr = 
        onEvent (fun (_: Dom.Element) (e: Dom.KeyboardEvent) (v: 'T) -> 
            if f (getKey e, v) then e.PreventDefault())

    let onKey key f = on.keyDown |> matchKey (function 
        | k when k = key -> f(); true 
        | _ -> false)

    let onKeyIfElse key view f f' =
        on.keyDownView view |> matchKeyValue (function 
            | (k, true) when k = key -> f(); true
            | (k, false) when k = key -> f'(); true
            | _ -> false)

    let onKeyIf key view f = onKeyIfElse key view f id

    let onEnter = onKey EnterKey
    let onEscape = onKey EscapeKey
    let onEnterIf = onKeyIf EnterKey
    let onEnterIfElse = onKeyIfElse EnterKey
    let onEscapeIf = onKeyIf EscapeKey
    let onEscapeIfElse = onKeyIfElse EscapeKey

    let onViewIf value f view = 
        on.viewUpdate view (fun e v -> if v = value then f e)

    let Focus (e : Dom.Element) = 
        let element = e :?> HTMLElement
        if element.TabIndex < 0 then 
            element.TabIndex <- 0
            Concurrency.Schedule element.Focus
        else
            element.Focus()

    let Blur (e : Dom.Element) = (e :?> HTMLElement).Blur()

module Lib =

    [<Stub>]
    [<Name("window.configureAnimations")>]
    let configureAnimations e = ()

    [<Stub>]
    [<Name("window.ready")>]
    let ready (selector: string) (f: #Dom.Element -> unit) = ()

    [<Name("NProgress")>]
    module Progress =

        [<Stub>]
        [<Name("start")>]
        let Start() = ()
        
        [<Stub>]
        [<Name("done")>]
        let Done() = ()

    module Sortable =
        type SortableStopEvent =
            {
                oldIndex: int
                newIndex: int
            }
        
        type CssSelector = string

        type MirrorOptions =
            {
                constrainDimensions: bool
                xAxis: bool
            }

        type SortableOptions =
            {
                draggable: CssSelector
                handle: CssSelector
                mirror: MirrorOptions
            }

        [<Stub>]
        [<Name "window.Sortable.default">]
        type Sortable(container: Dom.Element, options: SortableOptions) =
            [<Inline "$this.on('sortable:start', $handler)">]
            member this.OnStart(handler: unit -> unit) = X<Sortable>
            [<Inline "$this.on('sortable:stop', $handler)">]
            member this.OnStop(handler: SortableStopEvent -> unit) = X<Sortable>
            [<Stub>]
            static member Plugins = X<JSObject>

        let private disablePlugins = lazy (
            JS.Delete Sortable.Plugins "Focusable"
            JS.Delete Sortable.Plugins "Announcement"
        )

        let Create (update: int * int -> unit) e = 
            disablePlugins.Force()

            let options = {
                draggable = ".items-panel__item"
                handle = ".items-panel__drag-handle"
                mirror = { constrainDimensions = true; xAxis = false }
            }

            let sortable = Sortable(e, options)
            sortable
                .OnStart(fun _ -> JS.Document.ActiveElement |> Blur)
                .OnStop(fun x -> update (x.oldIndex, x.newIndex))
                |> ignore

    [<Stub>]
    module SignalR =
        [<Name "signalR.HubConnection">]
        type HubConnection() =
            member _.on(name: string, callback: 'A -> unit) = ()
            member _.start() = X<Promise<unit>>
            member _.invoke(name: string) = X<Promise<'R>>
            member _.invoke(name: string, arg: 'A) = X<Promise<'R>>
            member _.invoke(name: string, arg1: 'A, arg2: 'B) = X<Promise<'R>>
            member _.invoke(name: string, arg1: 'A, arg2: 'B, arg3: 'C) = X<Promise<'R>>
        [<Name "signalR.HubConnectionBuilder">]
        type HubConnectionBuilder() =
            member _.withUrl(url) = X<HubConnectionBuilder>
            member _.build() = X<HubConnection>

type Configuration =
    {
        RemotingEndpoint: string
        SignalREndpoint: string
    }

let ReloadFromServer () = JS.Window.Location.Reload(noCache = true)

let Init configuration (push : string -> unit) : unit =

    Lib.ready "input" (fun (e: HTMLInputElement) ->
        e.AddEventListener ("focus", fun () -> if not e.Value.IsNullOrWhitespace then e.Select())
        e.AddKeyDownHandler (function Keyboard EscapeKey -> e.Clear() | _ -> ())
    )

    WebSharper.Remoting.UseAjaxProviderWithErrorHandling()
    WebSharper.Remoting.EndPoint <- configuration.RemotingEndpoint

    let url = configuration.SignalREndpoint
    let connection = Lib.SignalR.HubConnectionBuilder().withUrl(url).build()
    connection.on("Push", push)
    connection.on("LiveReload", Mvu.LiveReload.Client.Reload)
    connection.start() |> ignore
