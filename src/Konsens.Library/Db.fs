module Konsens.Db

open LiteDB
open LiteDB.FSharp

open Konsens.Utils
open Konsens.Domain
open Konsens.Domain.Model
open Konsens.Utils.Reflection

type WithDb<'r> = Implicit<LiteDatabase,'r>

let getCollection name : WithDb<LiteCollection<'T>> = 
    Implicit.create(fun (db: LiteDatabase) -> db.GetCollection<'T>(name))

module Option =
    let ofObj o = if obj.ReferenceEquals(o, null) then None else Some o

[<System.AttributeUsage(System.AttributeTargets.Class)>]
type private CollectionAttribute(name) =
    inherit System.Attribute() with
    member _.Name : string = name

type private C<'T> = LiteCollection<'T>

let private useCollection (f: C<'T> -> 'r) : WithDb<'r> =
    Implicit.Do {
        let name = GetAttribute<'T, CollectionAttribute>().Name
        let! collection = getCollection name
        return f collection
    }

let private findById (id: Id) (collection: C<'T>) =
    let bsonId = BsonValue(id)
    collection.FindById bsonId |> Option.ofObj

let private findOne predicate (collection: C<'T>) =
    collection.FindOne(predicate |> toLinq) |> Option.ofObj

let private findAll (collection: C<'T>) = collection.FindAll()

module EventStore =

    [<Collection("events")>]
    type EventStream<'T> = 
        {
            Id: Id;
            Aggregate: string;
            Events: 'T list
        }

    let private queryEvents id (collection: C<EventStream<'T>>) =
        collection |> findById id |> Option.map (fun x -> x.Events) |?? []  

    let loadEvents (id: Id) = useCollection (queryEvents id)

    let saveEvents (id: Id) (version: int) (events: 'T list) = 
        useCollection (fun collection -> 
            let existingEvents = queryEvents id collection
            if existingEvents.Length <> version then failwith "Concurrency error"
            
            let newEvents = existingEvents @ events
            let aggregate = GetAttribute<'T, AggregateAttribute>().Name
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
            let model = collection |> findById id |> Option.defaultWith (fun () -> factory id)
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
            Code: string
        }

    let init = useCollection (fun (c: C<AbstimmungReadModel>) ->
        c.EnsureIndex(fun x -> x.Code) |> ignore
    )

    let neu id = { AbstimmungReadModel.Id = id; Titel = ""; Vorschläge = []; Code = "" }

    let apply (rm: AbstimmungReadModel) = function
    | Angelegt a -> { rm with Titel = a.Titel; Vorschläge = a.Vorschläge; Code = a.Code }
    | Abgestimmt _ -> rm
    | _ -> rm

    let getAll : WithDb<AbstimmungReadModel seq> = useCollection findAll

    let get id : WithDb<AbstimmungReadModel option> = useCollection (findById id)

    let getIdByCode code : WithDb<Id option> =
        useCollection (fun c -> 
            c 
            |> findOne <@ fun (x: AbstimmungReadModel) -> x.Code = code @>
            |> Option.map (fun { Id = id } -> id)        
        )


let dispatch (AbstimmungId id) (e: Domain.Events.Event list) =
    ReadStore.update id e Abstimmungen.apply Abstimmungen.neu

let loadEvents (AbstimmungId id) = EventStore.loadEvents id
let saveEvents (AbstimmungId id) = EventStore.saveEvents id

let private initializeDb = [ Abstimmungen.init ]

module Configuration =

    open Microsoft.Extensions.DependencyInjection

    let configure (path: string) =
        let mapper = FSharpBsonMapper()
        let sharedInstance = lazy (
            let db = new LiteDatabase(path, mapper)
            Seq.iter (Implicit.run db) initializeDb
            db
        )

        sharedInstance

    type IServiceCollection with
        member this.AddLiteDb(path: string) =
            let db = configure path
            this.AddSingleton<LiteDatabase>(fun _ -> db.Value)
        