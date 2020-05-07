namespace Konsens.Utils

type Implicit<'Dep,'Out> = private Implicit of ('Dep -> 'Out)

[<RequireQualifiedAccess>]
module Implicit =

    // basic operations

    let create = Implicit
    
    let run dep (i: Implicit<_,_>) = let (Implicit x) = i in x dep
    let constant (c : 'T) : Implicit<_,'T> = create (fun _ -> c)

    // lifting of functions and state

    let lift1 (f : 'd -> 'a -> 'out)
              : 'a -> Implicit<'d, 'out> =
        fun a -> create(fun dep -> f dep a)

    let lift2 (f : 'd -> 'a -> 'b -> 'out)
              : 'a -> 'b -> Implicit<'d, 'out> =
        fun a b -> create(fun dep -> f dep a b)

    let lift3 (f : 'd -> 'a -> 'b -> 'c -> 'out)
              : 'a -> 'b -> 'c -> Implicit<'d, 'out> =
        fun a b c -> create(fun dep -> f dep a b c)

    let liftDep (proj : 'd2 -> 'd1) 
                (Implicit rm : Implicit<'d1, 'output>) 
                : Implicit<'d2, 'output> =
        create (proj >> rm)
            
    // functor

    let fmap (f : 'a -> 'b) 
             (g : 'c -> 'a) 
             : ('c -> 'b) =
        g >> f

    let map (f : 'a -> 'b) 
            (Implicit i : Implicit<'d, 'a>) 
            : Implicit<'d,'b> =
        create (i >> f)

    let (<?>) = map

    // applicative-functor
        
    let apply (f : Implicit<'d, 'a->'b>)
              (i : Implicit<'d, 'a>)
              : Implicit<'d, 'b> =
        fun (dep: 'd) ->
            let f' = run dep f
            let a  = run dep i
            f' a
        |> create

    let (<*>) = apply

    // monad

    let bind (Implicit i : Implicit<'d, 'a>) 
             (f : 'a -> Implicit<'d,'b>) 
             : Implicit<'d, 'b> =
        fun dep ->
            f (i dep) 
            |> run dep 
        |> create

    let (>>=) = bind

    type ImplicitBuilder internal () =
        member __.Bind(m, f)    = m >>= f
        member __.Return(v)     = constant v
        member __.ReturnFrom(v) = v
        member __.Delay(f)      = f ()

    let Do = ImplicitBuilder()

[<WebSharper.JavaScript; AutoOpen>]
module Operators =
    let inline (|??) a b = a |> Option.defaultValue b

[<WebSharper.JavaScript; AutoOpen>]
module Extensions =

    type System.String with
        member s.IsNullOrWhitespace = System.String.IsNullOrWhiteSpace s

    module List =
        let move oldIndex newIndex list =
            list
            |> List.indexed
            |> List.sortBy (fun (index, _) -> 
                match index with 
                | i when i = oldIndex -> newIndex
                | i when i > oldIndex && i <= newIndex -> i - 1
                | i when i >= newIndex && i < oldIndex -> i + 1
                | i when i = newIndex -> oldIndex 
                | i -> i)
            |> List.map (fun (_, item) -> item)

module Reflection =
    open System
    open System.Linq.Expressions
    open Microsoft.FSharp.Quotations
    open FSharp.Linq.RuntimeHelpers

    let GetAttribute<'T, 'TAttr> () =
        typeof<'T>.GetCustomAttributes(typeof<'TAttr>, true)
        |> Seq.exactlyOne
        :?> 'TAttr

    let toLinq (expr : Expr<'a -> 'b>) =
        let linq = LeafExpressionConverter.QuotationToExpression expr
        let call = linq :?> MethodCallExpression
        let lambda = call.Arguments.[0] :?> LambdaExpression
        Expression.Lambda<Func<'a, 'b>>(lambda.Body, lambda.Parameters) 

module Crypto =
    open System
    open System.Text
    open System.Security.Cryptography

    let GetUniqueKey length =
        let chars = "ABCDEFGHIJKLMNPQRSTUVWXYZ123456789".ToCharArray()
        let data = Array.zeroCreate (length * 4)
        use provider = new RNGCryptoServiceProvider()
        provider.GetBytes(data)
        let result = StringBuilder()

        for i = 0 to length - 1 do
            let rnd = BitConverter.ToUInt32(data, i * 4)
            let idx = rnd % uint32(chars.Length)
            result.Append(chars.[int(idx)]) |> ignore

        result.ToString()
