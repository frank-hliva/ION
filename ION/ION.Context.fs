module ION.Context

open ION

let root = { Scope = Map.empty; Parent = None; Result = null }

let rec tryGet name (context : context) =
    match context.Scope.TryFind name with
    | Some value -> Some value
    | _ ->
        match context.Parent with
        | Some parent -> parent |> tryGet name
        | _ -> None

let get name context =
    match context |> tryGet name with
    | Some value -> value
    | _ -> failwith (sprintf "The value '%s' is not defined" name)

let set name value context =
    { context with Scope = context.Scope |> Map.add name value }