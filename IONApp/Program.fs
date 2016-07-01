open System.IO

open ION
open ION.Parsing
open ION.Evaluation

[<EntryPoint>]
let main argv =
    argv
    |> Array.iter
        ((fun path -> 
            if path.Contains "." then path
            else sprintf "%s.ion" path)
         >> File.ReadAllText
         >> evalString
         >> ignore)
    0