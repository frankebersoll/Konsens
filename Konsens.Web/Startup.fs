namespace PRChecklist.Web

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open WebSharper.AspNetCore
open Konsens.Web.Server

type Startup() =

    let [<Literal>] IndexFileName = "index.html"

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then app.UseDeveloperExceptionPage() |> ignore
        
        let indexHtml = System.IO.Path.Combine (env.WebRootPath, IndexFileName)

        app
            .UseDefaultFiles()
            .UseStaticFiles()

            .Map(PathString ClientConfiguration.RemotingUrl, fun app -> 
                app.UseWebSharper(fun builder ->
                    builder
                        .UseRemoting(true)
                        .UseSitelets(false)
                        |> ignore) 
                |> ignore)

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
