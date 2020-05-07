namespace PRChecklist.Web

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http

open WebSharper.Mvu.LiveReload
open WebSharper.AspNetCore
open WebSharper.Web.Remoting

open Konsens.Db.Configuration
open Konsens.Web

type LiveReloadPusher(context: Server.Push.PushContext) =
    interface ILiveReloadPusher with
        member this.Push(path) = Server.Push.LiveReload context path

type Startup(env: IWebHostEnvironment) =

    let [<Literal>] IndexFileName = "index.html"

    member _.ConfigureServices(services: IServiceCollection) =
        
        services
            .AddLiveReload<LiveReloadPusher>(env.WebRootFileProvider, ["Styles.css"; "index.html"])
            .AddLiteDb("Data.db")
            .AddSignalR() |> ignore

    member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        if env.IsDevelopment() then 
            app.UseDeveloperExceptionPage() |> ignore
        
        let indexHtml = System.IO.Path.Combine (env.WebRootPath, IndexFileName)
        let clientConfig = Konsens.Web.App.ClientConfiguration

        let useRemoting (app: IApplicationBuilder) =
            app
                .UseRemotingErrorHandling()
                .UseWebSharper(fun builder -> 
                    builder
                        .UseRemoting(true)
                        .UseSitelets(false) 
                        |> ignore) 
                    |> ignore

        app
            .UseDefaultFiles()
            .UseStaticFiles()
            .Map(PathString clientConfig.RemotingEndpoint, fun app -> useRemoting app)
            .UseRouting()
            .UseEndpoints(fun endpoints ->
                endpoints.MapHub<Server.Push.KonsensHub>(clientConfig.SignalREndpoint) |> ignore
            )
            .Run(fun ctx ->
                ctx.Response.ContentType <- "text/html";
                ctx.Response.SendFileAsync(indexHtml)
            )        
        |> ignore

module Program =

    [<EntryPoint>]
    let main args =
        WebHost
            .CreateDefaultBuilder(args)
            .UseStartup<Startup>()
            .Build()
            .Run()
        0
