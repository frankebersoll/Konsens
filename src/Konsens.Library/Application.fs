module Konsens.Application

open Konsens.Domain.Model

[<WebSharper.JavaScript>]
module Model =

    let CodeLength = 5

    type AbstimmungInfo = 
        {
            Id : AbstimmungId
            Code : string
            Titel : string
        }

open Konsens.Domain.Commands
open Model

module Implicit = Utils.Implicit

let private execute (command: Command) =
    Implicit.Do {
        let id = command.Id
        let! events = Db.loadEvents id
        let result = execute events command
        do! Db.saveEvents id result.Version result.Events
        do! Db.dispatch id result.Events
    }

module Abstimmung =
    let Neu titel vorschläge =
        let id = AbstimmungId.Neu()
        let code = Utils.Crypto.GetUniqueKey Model.CodeLength
        let command = CommandType.Anlegen {
            Titel = titel
            Vorschläge = vorschläge
            Benutzername = "asdf"
            Code = code            
        }

        Implicit.Do {
            do! execute (cmd id command)
            return id
        }

    let Get id =
        Implicit.Do {
            let! abstimmungen = Db.Abstimmungen.get id
            return
                abstimmungen
                |> Option.map (fun rm -> { 
                    AbstimmungInfo.Id = AbstimmungId id
                    Code = rm.Code
                    Titel = rm.Titel })
        }

    let GetIdByCode code = Db.Abstimmungen.getIdByCode code
