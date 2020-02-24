module Lexer

open Position // for position counting
open Mapping
open Token

// a special case of Transformation mapping
// this are used to make sure no cross contamitions happens
type 'Token Lexer = Lexer of (char list * position -> Result<'Token * (char list * position), string>)


let Lex (Lexer l) = l

// both error functions are inlined to make them generic
let inline EndOfStreamError expect =
    "Lexer Error : Expected " + string expect + " but reached end of stream.\n"
    |> Error

let inline LexError expect got pos =
    "Lexer Error : Expected " + string expect + " but got " + string got + " at position (" + string pos.line + ", " + string pos.offset + ").\n" 
    |> Error


let LexBind token =
    let bind (lst, pos) =
        match lst with
        | _ -> Ok ((token, pos), (lst,pos))
    Lexer bind

let LexExpect (symbol, token, moveFunc) =
    let expect (lst, pos) =
        match lst with
        | [] -> EndOfStreamError symbol
        | c :: cs when c = symbol -> Ok ((token, pos),(cs, Next pos))
        | c :: _ -> LexError symbol c pos
    Lexer expect 

let LexOr (Lexer l1) (Lexer l2) =
    Mapping l1 <|> Mapping l2
    |> Run
    |> Lexer

let ( <||> ) = LexOr // overshadowing the Transformation operator

let LexChoose lexlst =
    List.map (fun (Lexer l) -> Mapping l) lexlst // transform a lexer to a mapping
    |> Choose // collaps the mappings to one 
    |> Run // unwrap the mapping function
    |> Lexer // wraps it into a Lexer type

let LexAnd (Lexer l1) (Lexer l2) =
    Run (Mapping l1 <&> Mapping l2)
    |> Lexer

let ( <&&> ) = LexAnd // overshadowing the Transformation operator


let LexMap f (Lexer l) =
    // since Map take a singleton as argument to the function
    // and we need to it to take a pair and apply it to the first element
    // we do this
    Map (fun (token, pos) -> (f token, pos)) (Mapping l)
    |> Run
    |> Lexer

let LexFilter p lex =
    let rec filter input =
        match Lex lex input with
        | Ok ((token, _), _) as ok when p token -> ok
        | Ok (_, rest) -> filter rest
        | Error msg -> Error msg
    Lexer filter
        

let LexApply funcAsLex paramAsLex =
    funcAsLex <&&> paramAsLex
    |> LexMap (fun ((f, _), pair) -> f pair) 

let ( =|> ) = LexApply // overshadowing the Transformation operator

let LexLift func lexAsParam1 lexAsParam2 =
    LexBind func =|> lexAsParam1 =|> lexAsParam2
    

let LexSequence lexlst =
    lexlst
    |> List.map (fun (Lexer l) -> Mapping l)
    |> Sequenize
    |> Run
    |> Lexer
    
    


// Atom pattern for a given atom
// used when construction Regex, NFA or DFA for specific languages
let Atom symbol =
    (atom symbol)
    |> List.map (fun pair -> LexExpect pair)
    |> LexSequence
   
// 
let Any =
    let any (lst, pos) =
        match lst with
        | [] -> EndOfStreamError "'any symbol'"
        | c :: cs -> Ok ((ATOM c, pos), (cs, Next pos))
    Lexer any

// used to generate the DFA for th relugar expression lanugage itself
let AnyAtom =
    let atom (lst, pos) =
        match lst with
        | [] -> EndOfStreamError "'"
        | ''' :: c :: ''' :: cs -> //an atom e.i. symbol are writen as character enclosed in ' example 'a'
            Ok ((ATOM c, Next pos),(cs, Move 2 pos))
        | c :: _ -> LexError "'any symbol'" c pos
    Lexer atom



// remove noise
let NoiseFilter lexer =
    LexFilter (fun x -> not <| (x = NOISE)) lexer

// This should yield a tokized version of the input with noise.
// This pattern is for the language of regular expression 
let RegexPattern =
    Tokens // look in Tokens.fs
    |> List.map (fun pair -> LexExpect pair)
    |> LexChoose
    |> LexOr AnyAtom
    

let LexNext pattern pos input = Lex (NoiseFilter pattern) (input, pos)


let LexAll pattern =
    let rec all acc input =
        match Lex (NoiseFilter pattern) input with
        | Ok (token, ([], pos)) -> Ok (List.rev (token :: acc), ([],pos))
        | Ok (token, rest) -> all (token :: acc) rest
        | Error msg -> Error msg

    Lexer (all [])

    
