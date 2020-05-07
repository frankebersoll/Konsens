#r "paket:
nuget Argu
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Argu
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open System
open System.Collections.Generic
open System.Threading
open System.Text
open System.Text.RegularExpressions

let pagesDir = "src/Konsens.Web/Pages"
let outDir = "src/Konsens.Web/wwwroot"
let indexHtml = "index.html"

module UserInput =
    open System.Threading.Tasks

    let waitForAnyKey (cancellationToken: CancellationToken) =
        let x = Task.Run(fun () -> System.Console.ReadKey())
        let tcs = TaskCompletionSource()
        use __ = cancellationToken.Register(fun () -> tcs.SetCanceled())
        Task.WhenAny(x, tcs.Task).Wait()

module Tasks =

    let waitFor file =
        TaskRunner.waitFor (fun () ->
            try
                System.IO.File.Open(file, System.IO.FileMode.Open, System.IO.FileAccess.Read).Dispose()
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
            failwith "Fehler bei der CSS-Kompilierung"

        Trace.tracef "CSS kompiliert nach %s.\n" outDir

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
            |> Option.map (failwithf "Template '%s' ist mehrfach vorhanden")
            |> ignore
            s

        pagesDir </> indexHtml
        |> File.read
        |> Seq.map (replaceTemplates >> validateTemplates)
        |> File.writeWithEncoding Encoding.UTF8 false (outDir </> indexHtml)

        Trace.tracef "HTML kompiliert nach %s.\n" outDir

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

Target.create "BuildApp" (fun { Context = { Arguments = args } } ->
    
    let cli = ArgumentParser.Create<BuildArgs>().ParseCommandLine(args |> List.toArray)
    let config = 
        if cli.Contains(<@ Release @>) then 
            DotNet.BuildConfiguration.Release 
        else 
            DotNet.BuildConfiguration.Debug
     
    let setOptions (o: DotNet.BuildOptions) = { o with Configuration = config } 

    DotNet.build setOptions "src/Konsens.Web/"
)

Target.create "BuildCSS" (fun _ -> Tasks.buildCss())

Target.create "BuildHTML" (fun _ -> Tasks.buildHtml())

Target.create "Build" ignore

Target.create "Watch" (fun { Context = { CancellationToken = ct } } ->
    let watch glob f = !! glob |> ChangeWatcher.run (fun changes -> 
        let change = Seq.head changes
        Trace.logf "Änderung in '%s'\n" change.Name
        Tasks.waitFor change.FullPath
        f()
    ) 
    let watchers = [
        watch (pagesDir </> "**/*.styl") Tasks.buildCss
        watch (pagesDir </> "**/*.html") Tasks.buildHtml
    ]
    
    Trace.log "Warte auf Änderungen..."
    UserInput.waitForAnyKey ct

    watchers |> Seq.iter (fun d -> d.Dispose())
)

"Clean" ?=> "BuildApp"
"Clean" ?=> "BuildHTML"
"Clean" ?=> "BuildCSS"

"Build" <== [
    "BuildApp"
    "BuildHTML"
    "BuildCSS" ]

"Watch" <== [
    "BuildHTML"
    "BuildCSS"
]

Target.runOrDefaultWithArguments "Build"
