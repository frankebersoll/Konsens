[<WebSharper.JavaScript>]
module WebSharper.Mvu.Logging

open WebSharper.JavaScript

let private getGroupName (json: obj) =
    let rec getName x =
        match JS.TypeOf x with
        | JS.Kind.Object ->
            let fields = x |> JS.GetFields
            match fields |> Array.tryHead with
            | Some ("type", name) ->
                if fields.Length = 2 then
                    string(name) + " / " + getName (snd fields.[1])
                else
                    string(name)
            | _ -> ""
        | JS.Kind.String -> "\"" + string(x) + "\""
        | _ -> string(x)

    getName json

let CreateLogger (encodeMsg: 'Msg -> obj) (encodeModel: 'Model -> obj) = 
    let mutable updateCount = 0
    fun (msg: 'Msg) (model: 'Model) ->
        let logMsg = encodeMsg msg
        let logModel = encodeModel model
        Console.Group ("Update %i: %s", updateCount, getGroupName logMsg)
        Console.Log logMsg
        Console.Log logModel
        Console.GroupEnd()
        updateCount <- updateCount + 1

