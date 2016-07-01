namespace ION.Functions

open System
open ION
open ION.Parsing.Ast

type Function(args : element list, body : element list) =
    let guid = lazy Guid.NewGuid()
    member f.Args = args
    member f.Body = body
    member f.Guid = guid.Value
    member f.Invoke(eval : Eval, parent : context, args : obj list) =
        let scope = 
            args
            |> List.map2(fun name arg -> (name |> Element.asIdentifier), arg) f.Args
            |> Map.ofList
        body |> eval { Scope = scope; Parent = Some parent; Result = null }

type ExtensionInvoker =
    static member Invoke(func : obj, args : obj array) =
        let argTypes = args |> Array.map(fun arg -> arg.GetType())
        let methodInfo = func.GetType().GetMethod("Invoke", argTypes)
        methodInfo.Invoke(func, args)

type Extension(func, paramArray) =
    member f.ParamArray : bool = paramArray
    member f.Function = func
    member f.Invoke(args : obj list) =
        let args =
            let args = args |> Array.ofList
            if f.ParamArray then [| args |> box |] else args
        ExtensionInvoker.Invoke(func, args)

type CustomExtension(func) =
    member f.Function = func
    member f.Invoke(eval : Eval, context : context, args : element list) =
        ExtensionInvoker.Invoke(func, [| eval; context; args |])