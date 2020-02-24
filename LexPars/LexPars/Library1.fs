namespace LexPars

open Transformation
open Parser
open Lexer





type Class1() as main = 
    do
        main.print

    member _.X = "F#"

    member _.parse lst = 
        Parse <| ParseRegex (LexAll RegexPattern) <| lst

    member this.print =
        [for c in "('a'|'b')*'a''b''b'" -> c]
        |> this.parse
        |> Result
        |> printfn "%A"
        