#load @"ION.fs"
#load @"ION.Tokens.fs"
#load @"ION.Parsing.fs"
#load @"ION.Context.fs"
#load @"ION.Functions.fs"
#load @"ION.Std.fs"
#load @"ION.Evaluation.fs"

open ION
open System

"""
(let for
    (fun (from to lambda)
        (lambda from)
        (if (= from to) () (for (+ from 1) to lambda))
    )
)

(let fill&Print
    (fun (char size)
        (for 0 size (fun (size)
            (print char)
        ))
    )
)

(let succ (fun n (+ n 1)))
(let pred (fun n (- n 1)))

(let max 5)

(for 0 max (fun size
    (fill&Print " " (- 5 size))
    (fill&Print "X" (+ (* 2 size) 15))
    (print nl)
))

(fill&Print " " 7)(print "Hello world!" nl)

(for 0 max (fun size
    (fill&Print " " size)
    (fill&Print "X" (+ (* (- 5 size) 2) 15))
    (print nl)
))

(print (succ (pred 2)))

""" |> Tokens.ofString |> Parsing.Ast.fromTokens |> Evaluation.eval Std.stdContext