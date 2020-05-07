namespace WebSharper.Mvu.Pages

open WebSharper

[<WebSharper.JavaScript>]
module Update =

    type Request<'CR,'PM> =
        | CommonRequest of 'CR
        | PageMessage of 'PM

    let UpdateX state f update =
        let result = update state
        Mvu.SetModel (fst result) + f (snd result)

    [<RequireQualifiedAccess>]
    module Out =

        type Message<'CMsg,'PMsg> = 
            private
            | CommonMessage of 'CMsg
            | PageMessage of 'PMsg

        type Dispatch<'CMsg,'PMsg> = Mvu.Dispatch<Message<'CMsg,'PMsg>>

        let PageMessage m = Message.PageMessage m
        let CommonMessage m = Message.CommonMessage m

        let Map s p = function
            | CommonMessage shared -> s shared
            | PageMessage page -> p page

    type PageDefinition<'Model,'Msg,'CommonModel,'CommonRequest,'CMsg,'PageModel,'PMsg> = {
        GetPageModel : 'Model -> 'PageModel option
        GetSharedModel : 'Model -> 'CommonModel
        UpdatePage : 'PMsg -> 'PageModel -> Mvu.Action<'Msg, 'Model>
        RunAction : 'PageModel -> 'CommonRequest -> Mvu.Action<'Msg, 'Model>
        MapDispatch : ('Msg -> unit) -> (Out.Message<'CMsg,'PMsg> -> unit)
    }
    
    type Action<'CMsg,'PMsg,'PModel> = Mvu.Action<Out.Message<'CMsg,'PMsg>,'PModel>

    let Define (pageStateGetter: 'Model -> 'PageState)
               (pageStateSetter: 'PageState -> 'Model -> 'Model)
               (sharedStateGetter: 'Model -> 'SharedState)
               (commonMsgCtor: 'CMsg -> 'Message)
               (modelCtor: 'PModel -> 'PageState) 
               (msgCtor: 'PMsg -> 'Message) 
               (update: Request<'CR,'PMsg> -> 'PModel -> Action<'CMsg,'PMsg,'PModel>)
               (modelMatch: 'PageState -> 'PModel option) =
    
        let rec mapAction mapModel mapDispatch update (action: Action<'CMsg,'PMsg,'PModel>) =
            let mapCmd f dispatch = f (mapDispatch dispatch)
            let mapUpdate f = fun s -> 
                match mapModel s with 
                    | Some p -> s |> update (f p)
                    | None -> s
            match action with
            | Mvu.SetModel x -> Mvu.UpdateModel (update x)
            | Mvu.UpdateModel x -> Mvu.UpdateModel (mapUpdate x)
            | Mvu.Command x -> Mvu.Command (mapCmd x)
            | Mvu.CommandAsync x -> Mvu.CommandAsync (mapCmd x)
            | Mvu.DoNothing -> Mvu.DoNothing
            | Mvu.CombinedAction xs -> 
                xs |> List.map (mapAction mapModel mapDispatch update) |> Mvu.CombinedAction
    
        let getPageModel state = state |> pageStateGetter |> modelMatch
        let updatePageModel pageModel state = state |> pageStateSetter (pageModel |> modelCtor)
        let mapMessage = Out.Map commonMsgCtor msgCtor
        let mapDispatch (dispatch: Mvu.Dispatch<'Message>) = fun m -> mapMessage m |> dispatch
        let mapAction = mapAction getPageModel mapDispatch updatePageModel
        let updatePage message pageModel = update (PageMessage message) pageModel |> mapAction 
        let runAction pageModel action = update (CommonRequest action) pageModel |> mapAction
        {
            GetPageModel = getPageModel
            GetSharedModel = sharedStateGetter
            UpdatePage = updatePage
            RunAction = runAction
            MapDispatch = mapDispatch
        }
    
[<WebSharper.JavaScript>]
module View =
    
    open WebSharper.UI
    open WebSharper.UI.Client

    open Update

    [<RequireQualifiedAccess>]
    module Page =

        type PageBuilder = private {
            Attributes: Attr list
            KeepInDom: bool
            UsesTransition: bool
        }

        let Build = { Attributes = []; KeepInDom = false; UsesTransition = false }

        let WithAttribute attr builder = { builder with Attributes = attr :: builder.Attributes }
        let KeepInDom builder = { builder with KeepInDom = true }
        let UsesTransition builder = { builder with UsesTransition = true }

        type Render<'Key,'PageModel,'CMsg,'PMsg,'Ctx,'Result> =
            ('Key -> Out.Dispatch<'CMsg,'PMsg> -> View<'PageModel> -> 'Ctx -> 'Result)

        type PageWithState<'Key,'PageModel,'CMsg,'PMsg,'Ctx,'Result> = {
            Builder: PageBuilder
            Render: Render<'Key,'PageModel,'CMsg,'PMsg,'Ctx,'Result>
        } 

        let Render render builder = 
            { Builder = builder; Render = render }

        let RenderSingle render builder = 
            let render (_: unit) = render
            { Builder = builder; Render = render }

        let Apply f page = 
            let b = page.Builder
            f b.Attributes b.KeepInDom b.UsesTransition page.Render

    let WithPageState commonState createContext mapResult page definition =

        let pageFactory attrs keepInDom usesTransition render definition =

            let render key dispatch (info: Mvu.RenderInfo<'Model>) = 
                let isActive = Var.Create false
                let pageState = Var.CreateWaiting()
                let sharedState =
                    info.Model 
                    |> View.Map definition.GetSharedModel
                    |> View.UpdateWhile commonState isActive.View

                let dispatch = dispatch |> definition.MapDispatch
                let setter = 
                    info.Model |> View.Map definition.GetPageModel |> View.MapCached (function
                        | Some state -> 
                            Var.Set isActive true
                            Var.Set pageState state
                            Doc.Empty
                        | None -> 
                            Var.Set isActive false
                            Doc.Empty)

                let context = createContext pageState.View info.IsReady dispatch

                render key dispatch pageState.View context
                |> mapResult sharedState dispatch
                |> Doc.Append setter.V

            Mvu.Page.Reactive (id, render, attrs, keepInDom, usesTransition)

        Page.Apply pageFactory page definition
