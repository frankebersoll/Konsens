module Konsens.Web.Server

open WebSharper
open Konsens
open Konsens.Domain.Commands
open Konsens.Domain.Model

let private execute = execute Db.loadEvents Db.saveEvents Db.dispatch
let private benutzer = System.Guid.Parse("FFFFFFFF-1111-1111-1111-111111111111")

[<Rpc>]
let Neu titel vorschläge =
    async {
        let id = AbstimmungId.Neu()
        execute 
            { 
                Command.Id = id; 
                Type = 
                    CommandType.Anlegen (
                        {
                            Titel = titel
                            Vorschläge = vorschläge
                            Benutzername = "asdf"
                        })
            }

        return id
    }

[<JavaScript>]
module ClientConfiguration =
    let RemotingUrl = "/rpc"
