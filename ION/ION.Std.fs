module ION.Std

open System
open System.Text
open ION
open ION.Functions
open ION.Parsing.Ast

let func_let (eval : Eval) (context : context) (args : element list) =
    let var, expr = args.[0], args.[1]
    let name = var |> Element.asIdentifier
    let value = [expr] |> eval context
    context |> Context.set name value

let func_fun (eval : Eval) (context : context) (args : element list) =
    match args with
    | funcArgs :: body ->
        let funcArgs =
            match funcArgs with
            | List list -> list
            | arg -> [arg]
        new Function(funcArgs, body)
    | _ -> failwith "Invalid function definition"

let func_if (eval : Eval) (context : context) (args : element list) =
    match args.Length with
    | 2 | 3 as len ->
        if [args.[0]] |> eval context |> unbox<bool> then [args.[1]] |> eval context
        else if len = 3 then [args.[2]] |> eval context else null
    | _ -> failwith "Invalid if function"

let func_print : obj array -> _ = Array.iter Console.Write

let func_list : obj array -> _ = List.ofArray

let func_guid (func : Function) = func.Guid.ToString()

let op_addition (arr : obj array) =
    match arr with
    | [||] -> box null
    | _ -> 
        match arr.[0] with
        | :? number ->
            arr |> Array.map unbox<number> |> Array.reduce (+) |> box
        | :? string ->
            arr |> String.Concat |> box
        | _ -> failwith "'+' - op_addition - invalid type"

let extensions =
    [
        "-", (box ((-) : number -> _ -> _));
        "*", (box ((*) : number -> _ -> _));
        "/", (box ((/) : number -> _ -> _));
        "=", (box (=));
        ">", (box (>));
        "<", (box (<));
        ">=", (box (>=));
        "<=", (box (<=));
        "and", (box (&&));
        "or", (box (||));
        "not", (box (not));
        "?guid", (box (func_guid));
    ]

let paramArrayExtensions =
    [
        "print", (box func_print)
        "list", (box func_list)
        "+", (box op_addition)
    ]

let customExtensions =
    [
        "let", (box func_let)
        "fun", (box func_fun)
        "if", (box func_if)
    ]

let vars =
    [
        "nl", (box Environment.NewLine)
        "true", (box true)
        "nil", (box false)
    ]

let stdContext =
        [
            extensions, fun (name, func) -> name, box (new Extension(box func, false))
            paramArrayExtensions, fun (name, func) -> name, box (new Extension(box func, true))
            customExtensions, fun (name, func) -> name, box (new CustomExtension(box func))
            vars, fun (name, value) -> name, box value
        ]
        |> List.map (fun (list, mapper) -> list |> List.map mapper)
        |> List.concat
        |> Map.ofList
        |> fun map -> { Scope = map; Parent = None; Result = None }

let context (eval : EvalString) =
    stdContext |> Context.set "eval" (box (new Extension(box eval, false)))