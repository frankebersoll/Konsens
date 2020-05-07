namespace Konsens.Web

open WebSharper
open WebSharper.Web
open Konsens.Domain.Model

[<JavaScript>]
module Routing =

    [<RequireQualifiedAccess>]
    [<NamedUnionCases "type">]
    type Page =
        | [<EndPoint "/">] Home
        | [<EndPoint "/neu">] Neu
        | [<EndPoint "/abstimmung">] Abstimmung of System.Guid
        with
        static member ofAbstimmung (AbstimmungId id) = Abstimmung id

    let Router = Sitelets.InferRouter.Router.Infer<Page>()

    let Link page = Router.Link page

[<JavaScript>]
module Shared =

    type PushMessage =
    | Ping

[<RequireQualifiedAccess>]
module Server =

    open Konsens
    open Konsens.Application
    open Routing
    open QRCoder

    module Push =
        open Microsoft.AspNetCore.SignalR
    
        type KonsensHub() = inherit Hub()
        type PushContext = IHubContext<KonsensHub>
        
        let LiveReload (context: PushContext) (file : string) =
            context.Clients.All.SendAsync("LiveReload", file) |> ignore

        let Get () =
            let context = Remoting.GetService<PushContext>()
            fun (message: Shared.PushMessage) -> 
                let json = WebSharper.Json.Serialize message
                context.Clients.All.SendAsync("Push", json) |> ignore

    let resolve (reader: Utils.Implicit<'d,'out>) =
        let service = Remoting.GetService<'d>()
        Utils.Implicit.run service reader

    let toAsync x = async { return x }

    [<Rpc>]
    let QRCode id =
        let path = Link (Page.Abstimmung id)
        let uri = sprintf "http://192.168.2.189%s" path

        let payload = PayloadGenerator.Url(uri)
        use generator = new QRCodeGenerator()
        let data = generator.CreateQrCode(payload)
        use qr = new SvgQRCode(data)
        qr.GetGraphic(
            System.Drawing.Size(100, 100), 
            sizingMode = SvgQRCode.SizingMode.ViewBoxAttribute)

    [<Rpc>]
    let Neu titel vorschläge = Abstimmung.Neu titel vorschläge |> resolve |> toAsync

    [<Rpc>]
    let GetAbstimmung (id: Id) = Abstimmung.Get id |> resolve |> toAsync

    [<Rpc>]
    let GetAbstimmungIdByCode (code: string) = 
        async {
            do! Async.Sleep (System.DateTime.Now.Millisecond)
            return Abstimmung.GetIdByCode code |> resolve
        }
