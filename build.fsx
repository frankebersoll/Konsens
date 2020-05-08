#r "paket:
nuget Argu
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.JavaScript.Yarn
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Argu
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.JavaScript
open System
open System.Collections.Generic
open System.Threading
open System.Text
open System.Text.RegularExpressions

let webProject = "src/Konsens.Web"

let pagesDir = webProject </> "Pages"
let outDir = webProject </> "wwwroot"
let indexHtml = "index.html"

module UserInput =
    open System.Threading.Tasks

    let waitForAnyKey (cancellationToken: CancellationToken) =
        let x = Task.Run(fun () -> System.Console.ReadKey())
        let tcs = TaskCompletionSource()
        use __ = cancellationToken.Register(fun () -> tcs.SetCanceled())
        Task.WhenAny(x, tcs.Task).Wait()

module Tasks =
    open System.IO

    let waitFor file =
        TaskRunner.waitFor (fun () ->
            try
                System.IO.File.Open(file, FileMode.Open, FileAccess.Read).Dispose()
                true
            with
                _ -> false) (TimeSpan.FromSeconds(1.)) 100 id |> ignore

    let buildCss () =
        let result = 
            [pagesDir </> "Styles.styl"; "-o"; outDir]
            |> CreateProcess.fromRawCommand "node_modules/.bin/stylus.cmd"
            |> CreateProcess.redirectOutput
            |> CreateProcess.disableTraceCommand
            |> Proc.run

        if result.ExitCode <> 0 then
            Trace.traceError result.Result.Error
            failwith "Error while compiling CSS"

        Trace.tracef "CSS compiled to %s.\n" outDir

    let buildHtml () =

        let evaluator = MatchEvaluator(fun m -> 
            let indent s = m.Groups.[1].Value + s
            let src = m.Groups.[2].Value
            seq { 
                yield "<!-- " + src + " -->"
                yield! File.read (pagesDir </> src) 
                yield "" }
            |> Seq.map indent
            |> String.toLines)
        let replaceTemplates s = 
            Regex.Replace(s, """^(\s*)<page-templates src="([^"]+)" \/>""", evaluator)

        let hash = HashSet<string>()
        let validateTemplates s =
            Regex.Matches(s, @"ws-(?:children-)?template=\""([^""]+)\""")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Groups.[1].Value)
            |> Seq.tryFind (hash.Add >> not)
            |> Option.map (failwithf "Template '%s' exists multiple times")
            |> ignore
            s

        pagesDir </> indexHtml
        |> File.read
        |> Seq.map (replaceTemplates >> validateTemplates)
        |> File.writeWithEncoding Encoding.UTF8 false (outDir </> indexHtml)

        Trace.tracef "HTML compiled to %s.\n" outDir

    let restoreYarn () = Yarn.install id
         
    let restoreLibs () =
        let options o = 
            o 
            |> DotNet.Options.withWorkingDirectory webProject
            |> DotNet.Options.withRedirectOutput true
        let result = DotNet.exec options "libman" "restore"
        if not (List.isEmpty result.Errors) then failwith "Error on libman restore"

    let buildApp config =
        let setOptions (o: DotNet.BuildOptions) = { o with Configuration = config } 
        DotNet.build setOptions webProject    
  
type BuildArgs = 
    | [<AltCommandLine "-r">] Release
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Release -> "build with Release configuration."

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs
)

Target.create "Restore" (fun _ ->
    Tasks.restoreYarn ()
    Tasks.restoreLibs ()
)

Target.create "BuildApp" (fun { Context = { Arguments = args } } ->
    let cli = ArgumentParser.Create<BuildArgs>().ParseCommandLine(args |> List.toArray)
    let config = 
        if cli.Contains(<@ Release @>) then 
            DotNet.BuildConfiguration.Release 
        else 
            DotNet.BuildConfiguration.Debug

    Tasks.buildApp config
)

Target.create "Watch" (fun { Context = { CancellationToken = ct } } ->
    let watch glob f = !! glob |> ChangeWatcher.run (fun changes -> 
        let change = Seq.head changes
        Trace.logf "Change in '%s'\n" change.Name
        Tasks.waitFor change.FullPath
        f()
    ) 
    let watchers = [
        watch (pagesDir </> "**/*.styl") Tasks.buildCss
        watch (pagesDir </> "**/*.html") Tasks.buildHtml
    ]
    
    Trace.log "Waiting for changes..."
    UserInput.waitForAnyKey ct

    watchers |> Seq.iter (fun d -> d.Dispose())
)

Target.create "BuildCSS" (fun _ -> Tasks.buildCss())
Target.create "BuildHTML" (fun _ -> Tasks.buildHtml())
Target.create "Build" ignore
Target.create "RestoreAndBuild" ignore

"Restore" 
    ?=> "BuildHTML" 
    ?=> "BuildCSS" 
    ?=> "BuildApp"

"RestoreAndBuild" <== [
    "Restore"
    "Build"
]

"Build" <== [
    "BuildApp"
    "BuildHTML"
    "BuildCSS"
]

"Watch" <== [
    "BuildHTML"
    "BuildCSS"
]

Target.runOrDefaultWithArguments "RestoreAndBuild"
