namespace WebSharper

[<JavaScript>]
module Remoting =

    open System
    open WebSharper
    open WebSharper.JavaScript
    open WebSharper.Remoting

    type private StatusCode = Microsoft.AspNetCore.Http.StatusCodes

    [<JavaScriptExport>]
    type AjaxError(msg, xhr: XMLHttpRequest) = 
        inherit Exception(msg)
        member _.StatusText = xhr.StatusText
        member _.StatusCode = xhr.Status
        member _.ResponseText = xhr.ResponseText

    [<Direct @"
        var xhr = new XMLHttpRequest();
        var csrf = document.cookie.replace(new RegExp('(?:(?:^|.*;)\\s*csrftoken\\s*\\=\\s*([^;]*).*$)|^.*$'), '$1');
        xhr.open('POST', $url, $async);
        if ($async == true) {
            xhr.withCredentials = true;
        }
        for (var h in $headers) {
            xhr.setRequestHeader(h, $headers[h]);
        }
        if (csrf) {
            xhr.setRequestHeader('x-csrftoken', csrf);
        }
        function k() {
            if (xhr.status == 200) {
                $ok(xhr.responseText)
            } else if ($csrf && xhr.status == 403 && xhr.responseText == 'CSRF') {
                $csrf();
            } else {
                var msg = 'Response status is not 200: ';
                var error = new Konsens.Utils.Remoting.Ajax.AjaxError.New(msg + xhr.status, xhr);
                $err(error);
            }
        }
        if ('onload' in xhr) {
            xhr.onload = xhr.onerror = xhr.onabort = k;
        } else {
            xhr.onreadystatechange = function () {
                if (xhr.readyState == 4) {
                    k();
                }
            };
        }
        xhr.send($data);">]
    let private ajax (async: bool) (url: Url) (headers: Headers) (data: Data)
        (ok: Data -> unit) (err: exn -> unit) (csrf: unit -> unit) = ()

    type private CustomXhrProvider [<JavaScript>] () =
        interface IAjaxProvider with

            [<JavaScript>]
            member this.Async url headers data ok err =
                ajax true url headers data ok err
                    (fun () -> ajax true url headers data ok err JS.Undefined)

            [<JavaScript>]
            member this.Sync url headers data =
                let res = ref Unchecked.defaultof<_>
                ajax false url headers data
                    (fun x -> res := x)
                    (fun e -> raise e)
                    (fun () ->
                        ajax false url headers data
                            (fun x -> res := x)
                            (fun e -> raise e)
                            JS.Undefined)
                !res

    let UseAjaxProviderWithErrorHandling() = WebSharper.Remoting.AjaxProvider <- CustomXhrProvider()
    
    let HandleExceptions errCtor upgradeHandler action =
        let handler f dispatch = async {
            try do! f dispatch
            with :? AjaxError as e ->
                let err = errCtor >> dispatch
                match e.StatusCode with
                | 0 -> 
                    err "Es konnte keine Verbindung zum Server hergestellt werden."
                | StatusCode.Status426UpgradeRequired -> 
                    upgradeHandler()
                | StatusCode.Status500InternalServerError ->
                    err "Es gibt ein Problem auf dem Anwendungsserver."
                | code -> 
                    err (sprintf "HTTP-Status %i (%s)" code e.StatusText)
        }

        let rec replace = function
            | Mvu.CommandAsync x -> Mvu.CommandAsync (handler x)
            | Mvu.CombinedAction actions -> actions |> List.map replace |> Mvu.CombinedAction
            | action -> action

        replace action

namespace WebSharper.Web

module Remoting =

    open System
    open Microsoft.AspNetCore.Http
    open Microsoft.AspNetCore.Builder

    let rec handleError (ex: System.Exception) (response: HttpResponse) =
        match ex.GetBaseException() with
        | :? System.AggregateException as e -> handleError e.InnerException response
        | e when e.Message.StartsWith("Remote method signature incompatible") -> 
            response.StatusCode <- 426; true
        | _ -> false

    type IApplicationBuilder with
        member this.UseRemotingErrorHandling() =
            this.Use(fun context next -> 
                async {
                    try do! next.Invoke() |> Async.AwaitTask
                    with e when handleError e context.Response -> ()
                }
                |> Async.StartAsTask 
                :> System.Threading.Tasks.Task)

    let private services = "WebSharper.AspNetCore.Services"

    let GetService<'T>() =
        let context = Remoting.GetContext()
        let serviceProvider = context.Environment.[services] :?> IServiceProvider
        serviceProvider.GetService(typeof<'T>) :?> 'T
