module Konsens.Db

open LiteDB
open LiteDB.FSharp
open Konsens.Domain
open Konsens.Domain.Model
open Konsens.Utils

let private mapper = FSharpBsonMapper()

[<System.AttributeUsage(System.AttributeTargets.Class)>]
type private CollectionAttribute(name) =
    inherit System.Attribute() with
    member _.Name : string = name

let private useCollection f =
    let name = Attributes.get<'T, CollectionAttribute>().Name
    use db = new LiteDatabase("simple.db", mapper)
    let collection = db.GetCollection<'T>(name)
    f collection

let private tryGet (id: Id) f defaultValue (collection: LiteCollection<'T>) =
    let bsonId = BsonValue(id)
    let result = collection.FindById bsonId
    if obj.ReferenceEquals(result, null) then
        defaultValue id
    else
        f result

module EventStore =

    [<Collection("events")>]
    type EventStream<'T> = 
        {
            Id: Id;
            Aggregate: string;
            Events: 'T list
        }

    let private queryEvents id (collection: LiteCollection<EventStream<'T>>) =
        collection |> tryGet id 
            (fun x -> x.Events) 
            (fun _ -> [])

    let loadEvents (id: Id) : 'T list = 
        useCollection (queryEvents id)

    let saveEvents (id: Id) (version: int) (events: 'T list) = 
        useCollection (fun collection -> 
            let existingEvents = queryEvents id collection
            if existingEvents.Length <> version then failwith "Concurrency error"
            
            let newEvents = existingEvents @ events
            let aggregate = Attributes.get<'T, AggregateAttribute>().Name
            let abstimmung = {
                Id = id
                Aggregate = aggregate
                Events = newEvents 
            }

            collection.Upsert(abstimmung) |> ignore    
        )

module ReadStore =
    let update (id: Id) (events: 'T list) apply factory =
        useCollection (fun collection ->
            let model = collection |> tryGet id Operators.id factory
            let result = events |> Seq.fold apply model
            if result <> model then
                collection.Upsert(BsonValue(id), result) |> ignore
        )

module Abstimmungen =
    open Domain.Events

    [<Collection("abstimmungen")>]
    type AbstimmungReadModel = 
        {
            Id: System.Guid
            Titel: string
            Vorschläge: string list
        }

    let neu id = { AbstimmungReadModel.Id = id; Titel = ""; Vorschläge = [] }

    let apply (rm: AbstimmungReadModel) = function
    | Angelegt a -> { rm with Titel = a.Titel; Vorschläge = a.Vorschläge }
    | Abgestimmt _ -> rm
    | _ -> rm

    let getAll(): AbstimmungReadModel seq = 
        useCollection (fun collection -> collection.FindAll())

let dispatch (AbstimmungId id) (e: Domain.Events.Event list) =
    ReadStore.update id e Abstimmungen.apply Abstimmungen.neu

let loadEvents (AbstimmungId id) = EventStore.loadEvents id
let saveEvents (AbstimmungId id) = EventStore.saveEvents id