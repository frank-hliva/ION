module ION.Parsing.Ast

open ION
open ION.Tokens

let rec private parse acc parents = function
| [] -> acc |> List.rev
| (token : token) :: tokens ->
    (match token with
    | token.Literal literal -> parse ((Literal literal) :: acc) parents
    | token.Symbol symbol -> parse ((Symbol symbol) :: acc) parents
    | token.LParen -> parse [] (acc :: parents)
    | token.RParen ->
        match parents with
        | [] -> failwith "Unexpected symbol ')'"
        | parent :: tail ->
            let list = acc |> List.rev |> List
            parse (list :: parent) tail) tokens

let fromTokens (tokens : token list) =
    tokens |> parse [] []