namespace ION

type token = 
| Symbol of string
| Literal of obj
| LParen
| RParen

and number = double

and Eval = context -> element list -> obj
and EvalString = string -> obj

and context = 
    { Scope : Map<string, obj>
      Parent : context option
      Result : obj }

and element =
| Symbol of string
| Literal of obj
| List of element list

module Element =

    let rec asIdentifier (element : element) =
        match element with
        | element.Symbol str -> str
        | _ -> failwith "Invalid identifier"