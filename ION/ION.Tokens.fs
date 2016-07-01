module ION.Tokens

open System
open System.Globalization

open ION

let private asString : char list -> _ = List.rev >> String.Concat

let private (|IsNumber|_|) (input : string) =
    let char = input.[0]
    if (Char.IsDigit char) || char = '-' then
        let isNum, num = Double.TryParse(input, NumberStyles.Any, CultureInfo.InvariantCulture)
        if isNum then Some num else None
    else None

let push value tokens =
    match value |> asString with
    | "" -> tokens
    | str ->
        (match str with
        | "(" -> LParen | ")" -> RParen
        | IsNumber num -> token.Literal (box num)
        | _ -> token.Symbol str) :: tokens

let pushString value tokens =
    token.Literal (value |> asString |> box) :: tokens

let rec private tokenize tokens token modes = function
| [] -> 
    match token with
    | [] -> tokens
    | _ -> tokens |> push token
| char :: chars ->
    let tokenize =
        match modes with
        | [] -> 
            match char with
            | ' ' | '\t' | '\n' | '\r' ->
                tokenize (tokens |> push token) [] modes
            | '(' | ')' ->
                let tokens = tokens |> push token |> push [char]
                tokenize tokens [] modes
            | '\"' | ';' ->
                tokenize (tokens |> push token) [] (char :: modes)
            | _ -> tokenize tokens (char :: token) modes
        | mode :: tail ->
            match mode with
            | '\"' ->
                match char with
                | '\"' -> tokenize (tokens |> pushString token) [] tail
                | '\\' -> tokenize tokens token (char :: modes)
                | _ -> tokenize tokens (char :: token) modes
            | '\\' ->
                match char with
                | '\"' | '\\' -> tokenize tokens (char :: token) tail
                | _ -> failwith "Invalid escape sequence"
            | ';' ->
                match char with
                | '\r' | '\n' -> tokenize tokens [] tail
                | _ -> tokenize tokens [] modes
            | _ -> failwith "Invalid mode"
    tokenize chars

let ofString : string -> _ =
    List.ofSeq >> tokenize [] [] [] >> List.rev