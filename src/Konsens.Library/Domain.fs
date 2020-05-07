namespace Konsens.Domain

[<System.AttributeUsage(System.AttributeTargets.Class)>]
type AggregateAttribute(name) =
    inherit System.Attribute() with
    member _.Name : string = name

[<WebSharper.JavaScript>]
module Model =

    type Id = System.Guid
    type Index = int
    type BenutzerId = BenutzerId of Id
    type AbstimmungId = AbstimmungId of Id
    type Benutzer = { Id: BenutzerId; Name: string; }
    type Bewertung = int
    type Stimme = { Benutzer: BenutzerId; Bewertung: Bewertung }
    type Vorschlag = { Titel: string; Stimmen: Stimme list }
    type Status = Neu | Offen | Beendet | Entfernt
    type Abstimmung = { Status: Status; Titel: string; Vorschläge: Vorschlag list; Benutzer: Benutzer list }

    module BenutzerId =
        let Neu () = BenutzerId (Id.NewGuid())

    module AbstimmungId =
        let Neu () = AbstimmungId (Id.NewGuid())

    module Abstimmung =
        let Neu = { Status = Neu; Titel = "(neu)"; Vorschläge = []; Benutzer = [] }

module Events =

    open Model

    type AngelegtEvent = { Titel: string; Vorschläge: string list; Code: string }
    type AbgestimmtEvent = { Index: Index; Benutzer: BenutzerId; Bewertung: Bewertung }

    [<Aggregate "Abstimmung">]
    type Event =
        | Angelegt of AngelegtEvent
        | Entfernt
        | Beigetreten of Benutzer
        | Abgestimmt of AbgestimmtEvent
        | Geschlossen

    let apply (agg: Abstimmung) event =

        let onAngelegt e =
            let createVorschlag titel = { Titel = titel; Stimmen = [] }
            let vorschläge = e.Vorschläge |> List.map createVorschlag

            { agg with Titel = e.Titel; Status = Offen; Vorschläge = vorschläge }

        let onAbgestimmt { Index = index; Benutzer = benutzer; Bewertung = bewertung} =
            let stimme = { Stimme.Benutzer = benutzer; Bewertung = bewertung }
            let updateIndex index f xs = 
                xs |> List.mapi (fun i x -> if i = index then f x else x)
            let addOrUpdateStimme (stimme: Stimme) (stimmen: Stimme list) =
                stimmen 
                |> Seq.map (fun x -> x.Benutzer, x) 
                |> Map.ofSeq
                |> Map.add stimme.Benutzer stimme
                |> Map.toList
                |> List.map (fun (_, value) -> value)

            let updateVorschlag stimme vorschlag =
                { vorschlag with Stimmen = addOrUpdateStimme stimme vorschlag.Stimmen }
                    
            let vorschläge = agg.Vorschläge |> updateIndex index (updateVorschlag stimme)

            { agg with Vorschläge = vorschläge }

        match event with
        | Angelegt e -> onAngelegt e
        | Entfernt -> { agg with Status = Status.Entfernt }
        | Beigetreten e -> { agg with Benutzer = agg.Benutzer @ [e] }
        | Abgestimmt e -> onAbgestimmt e
        | _ -> agg

module Commands =

    open Model

    type AnlegenCommand = { Titel: string; Benutzername: string; Vorschläge: string list; Code: string }

    type CommandType =
        | Anlegen of AnlegenCommand
        | Beitreten of string
        | Entfernen
        | Abstimmen of Index * BenutzerId * Bewertung

    type Command = { Id: AbstimmungId; Content: CommandType }

    let cmd id content = { Id = id; Content = content }

    type ExecutionResult = { Version: int; Events: Events.Event list }

    let private handleAnlegen cmd agg = 
        match agg with
        | { Status = Neu } -> 
            [ 
                Events.Angelegt { Titel = cmd.Titel; Vorschläge = cmd.Vorschläge; Code = cmd.Code }
                Events.Beigetreten { Id = BenutzerId.Neu(); Name = cmd.Benutzername }
            ]
        | _ -> failwith "Existiert bereits"

    let private handleBeitreten name agg =
        let nameExistiert = agg.Benutzer |> Seq.exists (fun b -> b.Name = name)
        match agg with
        | { Status = Offen } ->
            if not nameExistiert
                then [ Events.Beigetreten { Id = BenutzerId.Neu(); Name = name } ]
                else failwith "Benutzer existiert bereits"
        | _ -> failwith "Kann nicht beitreten"

    let private handleEntfernen agg =
        match agg with
        | { Status = Neu | Entfernt } -> []
        | _ -> [ Events.Entfernt ]

    let private handleAbstimmen index benutzerId bewertung agg =
        let benutzerExistiert = agg.Benutzer |> Seq.exists (fun b -> b.Id = benutzerId)
        match agg with
        | { Status = Offen } when benutzerExistiert -> 
            [ Events.Abgestimmt { Index = index; Benutzer = benutzerId; Bewertung = bewertung} ]
        | _ -> []

    let private handle command agg =
        match command with
        | Anlegen cmd -> handleAnlegen cmd agg
        | Beitreten name -> handleBeitreten name agg
        | Entfernen -> handleEntfernen agg
        | Abstimmen (index, benutzerId, bewertung) -> handleAbstimmen index benutzerId bewertung agg

    let execute events (command: Command) =
        let version = events |> List.length
        let agg = events |> List.fold Events.apply Abstimmung.Neu
        let result = agg |> handle command.Content
        result |> List.fold Events.apply agg |> ignore
        { ExecutionResult.Version = version; Events = result }