module WebSharper.Mvu.LiveReload

open System
open System.Threading.Tasks
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Primitives
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection

open WebSharper

type ILiveReloadPusher =
    abstract member Push: path:string -> unit

type private Configuration = {
    FileProvider: IFileProvider
    Files: string list
}

type private Watcher(configuration: Configuration, pusher: ILiveReloadPusher) = 
    let mutable disposables = []
    let watch filetype = 
        let fileProvider = configuration.FileProvider
        let createToken = Func<IChangeToken>(fun () -> fileProvider.Watch filetype)
        let onChange () = pusher.Push("/" + filetype)
        ChangeToken.OnChange(createToken, Action(onChange))

    let start () = disposables <- configuration.Files |> List.map watch
    let stop () = disposables |> List.iter (fun d -> d.Dispose())

    interface IHostedService with
        member _.StartAsync _ = start(); Task.CompletedTask
        member _.StopAsync _ = stop(); Task.CompletedTask
    
type IServiceCollection with
    member this.AddLiveReload<'T when 'T :> ILiveReloadPusher and 'T : not struct>(fileProvider, files) =
        this.AddTransient<ILiveReloadPusher, 'T>()
            .AddSingleton({ Configuration.FileProvider = fileProvider; Files = files })
            .AddSingleton<IHostedService, Watcher>()

[<JavaScript>]
module Client =
        
    open WebSharper.JavaScript
    open WebSharper.UI

    [<Inline "new DOMParser().parseFromString($html, 'text/html')">]
    let private Parse html = X<Dom.Document>

    [<Direct "WebSharper.UI.Client.Templates.LoadLocalTemplates($baseName)">]
    let private LoadLocalTemplates baseName = ()

    let private ReloadLocalTemplates baseName =
        let templates = JS.Global.GetJS("StartupCode$WebSharper_UI$Templates")
        templates.SetJS("LocalTemplatesLoaded", false)
        let dict = templates.GetJS("LoadedTemplates") :?> System.Collections.IDictionary
        dict.Clear()
        LoadLocalTemplates baseName

    let private updateVar = Var.Create 0
        
    let View = updateVar.View

    let GetVersion() = View |> UI.View.TryGet |> Option.defaultValue 0
        
    let Reload (file : string) =
        let trigger () = updateVar.Update (fun x -> x + 1)
        let fileDot = file.LastIndexOf(".")
        let extension = file.Substring(fileDot)
        let filename = file.Substring(0, fileDot)
        match extension with
        | ".css" ->
            let selector = sprintf "head>link[href^='%s']" file
            let link = JS.Document.QuerySelector(selector)
            let href = sprintf "%s?%s" file (System.Guid.NewGuid().ToString("N"))
            link.SetAttribute("href", href)
        | ".html" ->
            promise { 
                let! response = JS.Fetch file
                let! text = response.Text()
                let newTemplates = (Parse text).GetElementById("ws-templates")
                let oldTemplates = JS.Document.GetElementById("ws-templates")
                oldTemplates.ParentElement.ReplaceChild(newTemplates, oldTemplates) |> ignore
                ReloadLocalTemplates filename
                trigger()
            } |> ignore
        | _ -> ()
