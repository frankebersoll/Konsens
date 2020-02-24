namespace Konsens.Web

open WebSharper
open WebSharper.Mvu
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.Sitelets
open WebSharper.JavaScript

open Konsens.Web.Model
open Konsens.Utils

[<JavaScript>]
module App =

    [<SPAEntryPoint>]
    let Main () =
        
        Client.Init ()

        App.CreatePaged State.Init Update.UpdateApp View.Page
        |> App.WithHtml5Routing View.Router (fun s -> s.Page) Update.Goto
        |> App.WithLog (fun msg model ->
            Console.Log (Json.Encode msg)
            Console.Log (Json.Encode model)
        )
        |> App.Run
        |> Doc.RunById "main"