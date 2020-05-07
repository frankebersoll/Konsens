namespace Konsens.Web

open WebSharper
open WebSharper.UI.Client
open WebSharper.Mvu

open Konsens.Web.Model
open Konsens.Web.Client

[<JavaScript>]
module App =

    let ClientConfiguration =
        {
            RemotingEndpoint = "/rpc"
            SignalREndpoint = "/signalr"
        }

    let encodeMsg = function
        | Update.CommonMessage msg -> msg |> Json.Encode
        | msg -> msg |> Json.Encode

    let encodeModel = Json.Encode<State>

    [<SPAEntryPoint>]
    let Main () =
        App.CreatePaged State.Init Update.UpdateApp View.Page
        |> App.WithInitMessage (Update.Startup ClientConfiguration)
        |> App.WithUpdateHandler View.HandleUpdate
        |> App.WithHtml5Routing Routing.Router 
            (fun s -> s.Page) 
            (fun dispatch x -> 
                Pages.Update.Goto x |> Update.CommonMessage |> dispatch)
        |> App.WithLog (Logging.CreateLogger encodeMsg encodeModel)
        |> App.WithRemoteDev (RemoteDev.Options(hostname = "localhost", port = 8000))
        |> App.Run
        |> Doc.RunById "main"