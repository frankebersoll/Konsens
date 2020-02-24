[<WebSharper.JavaScript>]
module Konsens.Web.Client

open WebSharper
open WebSharper.JavaScript
open Konsens.Utils

type KeyboardKey = EnterKey | EscapeKey

let (|Keyboard|_|) (e: Dom.KeyboardEvent) = 
    match e.KeyCode with
    | 13 -> Some EnterKey
    | 27 -> Some EscapeKey
    | _ -> None

[<Direct("window.ready($selector, $f);")>]
let ready selector (f: #Dom.Element -> unit) = ()

type Dom.Element with
    [<Inline("$el.scrollIntoView()")>]
    member el.ScrollIntoView () = ()

type Dom.EventTarget with
    member e.AddKeyDownHandler f = e.AddEventListener ("keydown", fun (e: Dom.Event) -> f (e :?> Dom.KeyboardEvent))

let Init () =

    WebSharper.Remoting.EndPoint <- Server.ClientConfiguration.RemotingUrl

    ready "input" (fun (e: HTMLInputElement) ->
        e.AddEventListener ("focus", fun () -> if not e.Value.IsNullOrWhitespace then e.Select())
        e.AddKeyDownHandler (function Keyboard EscapeKey -> e.Value <- "" | _ -> ())
    )
