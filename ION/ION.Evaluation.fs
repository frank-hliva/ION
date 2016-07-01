module ION.Evaluation

open ION
open ION.Tokens
open ION.Parsing
open ION.Functions

let rec eval (ctx : context) list =

    let getObject name = 
        match ctx |> Context.tryGet name with
        | Some value -> value
        | _ -> failwith (sprintf "Variable '%s' is not defined" name)

    let evalList = List.map (fun element -> [element] |> eval ctx)

    let fetch args (value : obj) =
        match value with
        | :? Function as func -> func.Invoke(eval, ctx, args |> evalList)
        | :? Extension as func -> func.Invoke(args |> evalList)
        | :? CustomExtension as func -> func.Invoke(eval, ctx, args)
        | value -> value

    match list with
    | [] -> ctx.Result
    | x :: xs ->
        match x with
        | element.Literal literal ->
            xs |> eval { ctx with Result = literal }
        | element.Symbol symbol ->
            let value = symbol |> getObject
            xs |> eval { ctx with Result = value } 
        | element.List list ->
            match list with
            | [] -> xs |> eval { ctx with Result = null }
            | head :: tail ->
                let value =
                    match head with
                    | Symbol symbol -> symbol |> getObject
                    | value -> [value] |> eval ctx
                match value |> fetch tail with
                | :? context as ctx -> xs |> eval { ctx with Result = null }
                | result -> xs |> eval { ctx with Result = result }

and evalString (input : string) =
    input
    |> Tokens.ofString
    |> Ast.fromTokens
    |> eval (Std.context evalString)