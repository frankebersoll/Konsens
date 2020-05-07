[<WebSharper.JavaScript>]
module Konsens.Web.Pages.Abstimmung

open WebSharper

open Konsens.Domain.Model
open Konsens.Application.Model

module Model =

    type AbstimmungState =
        {
            Id : AbstimmungId
            Abstimmung : AbstimmungInfo
        }
    
    module AbstimmungState =
        let Create id = {
            Id = id
            Abstimmung = { AbstimmungInfo.Id = id; Code = ""; Titel = "" }
        }
    
        let SetAbstimmung abstimmung (state: AbstimmungState) =
            { state with Abstimmung = abstimmung }

open WebSharper.Mvu
open WebSharper.Mvu.Pages
open Konsens.Web

module Update =

    open Model
    open Update

    [<NamedUnionCases "type">]
    type AbstimmungMessage =
        | AbstimmungLoaded of AbstimmungInfo

    type Dispatch = Out.Dispatch<AbstimmungMessage>

    let private LoadAbstimmung (AbstimmungId id) (dispatch: Out.Dispatch<AbstimmungMessage>) =
        async {
            let! result = Server.GetAbstimmung id
            match result with
            | Some info -> AbstimmungLoaded info |> Out.PageMessage |> dispatch
            | None -> Out.Error "Abstimmung nicht gefunden" |> dispatch
        }

    let UpdateAbstimmung (message: Request<AbstimmungMessage>) (pageState: AbstimmungState) =
        match message with
        | CommonRequest action ->
            match action with
            | Init -> LoadAbstimmung pageState.Id |> CommandAsync
            | Push _ -> DoNothing
            | RecoverFromError -> DoNothing
            | ResetFocus -> DoNothing
        | PageMessage message ->
            match message with
            | AbstimmungLoaded info -> AbstimmungState.SetAbstimmung info |> UpdateModel
    
module View =

    open WebSharper.UI

    open Client
    open Model
    open Update
    open View

    type Template = Templating.Template<"Pages/Abstimmung/Abstimmung.html", Templating.ClientLoad.FromDocument>
    
    type Context = Page.Context<AbstimmungState, AbstimmungMessage>

    let Page = 
        Page.Build
        |> Page.Render (fun key (dispatch: Dispatch) (page: View<AbstimmungState>) (ctx: Context) ->

            let qrcode = Server.QRCode key

            Template.Abstimmung()
                    .Titel(page.V.Abstimmung.Titel)
                    .Code(page.V.Abstimmung.Code)
                    .QRCode(Doc.Verbatim(qrcode))
                    .Doc()

            |> WithLayout (DefaultLayout { Icon = Template.AbstimmungIcon().Doc(); Subtitle = (View.Const None) })
        )
