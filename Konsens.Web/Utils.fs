namespace Konsens.Utils

open WebSharper

[<AutoOpen; JavaScript>]
module Extensions =
    type System.String with
        member s.IsNullOrWhitespace = System.String.IsNullOrWhiteSpace s

module Attributes =
    let get<'T, 'TAttribute> () =
        typeof<'T>.GetCustomAttributes(typeof<'TAttribute>, true)
        |> Seq.exactlyOne
        :?> 'TAttribute

module Doc =
    open QRCoder
    open System
  
    let QRCode uri=
        use generator = new QRCodeGenerator()
        let payload = PayloadGenerator.Url(uri)
        let data = generator.CreateQrCode(payload)
        use qr = new SvgQRCode(data)
        qr.GetGraphic(
            Drawing.Size(100, 100), 
            sizingMode = SvgQRCode.SizingMode.ViewBoxAttribute)
        |> UI.Doc.Verbatim

[<JavaScript>]
module App =
    open WebSharper.JavaScript
    open WebSharper.Mvu
    open WebSharper.UI

    let [<Literal>] private VarField = "Var"
    let [<Literal>] private InitField = "Init"

    let WithHtml5Routing<'Route, 'Message, 'Model, 'Rendered when 'Route : equality>
            (router: WebSharper.Sitelets.Router<'Route>)
            (getRoute: 'Model -> 'Route)
            (setRoute: 'Route -> 'Model -> 'Model)
            (app: App<'Message, 'Model, 'Rendered>) =
        let var = app.GetJS(VarField) :?> Var<'Model>
        let init = app.GetJS(InitField) :?> Dispatch<'Message> -> unit
        let lensedRoute = var.Lens getRoute (fun m r -> setRoute r m)
        let newInit = fun dispatch ->
            init dispatch
            let defaultRoute = getRoute var.Value
            Router.InstallInto lensedRoute defaultRoute router
        app.SetJS(InitField, newInit)
        app
