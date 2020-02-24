module Parser

open Lexer
open Mapping
open Regex
open Token
open Position

let private asciiprintable = [for i in 32 .. 126 -> char i ]

// special case of transformation mapping
type Parser<'input, 'symbol, 'output> = Parser of ('input -> Result<'output * (int * ('symbol RegToken * position) list),  string>)

let Parse (Parser p) = p


let inline ParserEndOfStream token = 
    "Parser Error : Expected " + string token + " but reached token end of stream."
    |> Error

let inline ParserError expect got pos = 
    "Parser Error : Expected " + string expect + " but got " + string got + " at position (" + string pos.line + ", " + string pos.offset + ")"
    |> Error


// count is used to mark all atoms and terminals with unique id
let ParserExpect token ret =
    let expect (count, input) =
        match input with
        | [] -> ParserEndOfStream token 
        | ((token', _) as pair) :: tokens when token = token' -> Ok (ret pair, (count, tokens))
        | (token', pos) :: tokens -> ParserError token token' pos
    
    Parser expect

let ParserBind output =
    let bind input =
        Ok (output, input)

    Parser bind

let ParserOr (Parser p1) (Parser p2) =
    Mapping p1 <|> Mapping p2
    |> Run
    |> Parser

let ( <?> ) = ParserOr

let ParserChoose plst =
    List.reduce ParserOr plst

let ParserAnd (Parser p1) (Parser p2) =
    Mapping p1 <&> Mapping p2
    |> Run
    |> Parser


let ( <!> ) = ParserAnd


let ParserMap f (Parser p) =
    Map f (Mapping p)
    |> Run
    |> Parser


let ParserApply funcAsParser paramAsParser =
    funcAsParser <!> paramAsParser
    |> ParserMap (fun (f, token) -> f token)


let ( =?> ) = ParserApply


let ParserLift2 f paramAsParser1 paramAsParser2 =
    ParserBind f =?> paramAsParser1 =?> paramAsParser2


let ParserSequence plst =
    plst
    |> List.map (fun (Parser p) -> Mapping p)
    |> Sequenize
    |> Run
    |> Parser



(*
    We need to make a parser for the following patterns

    an atom e.i. 'a'
    an enclosed regex e.i. ( regex )
    an Interval
    a complement of interval
    a complement of a sequence          : OBS need to establish how this is writen
    a star regex e.i. regex*
    a plus regex e.i. regex+
    an or regex e.i. regex1 | regex2
    a cons regex e.i. regex1 regex2

    more patterns can easiely be added in the ParseNext

    operators as / ^, ~, $, !, regex{0,n} and more will be added later,
    with explainations
*)

let ParseAtom =
    let atom (count, tokens) =
        match tokens with
        | [] -> ParserEndOfStream "An Atom"
        | (ATOM a, pos) :: tokens' ->
            (Atom (a, count, pos), (count + 1, tokens'))
            |> Ok
        | (token, pos) :: _ ->
            ParserError "An atom" token pos

    Parser atom

// to parse for possibly multiply ested regex we need to give it a parser e.i. the recursive call to the outermost parser
let ParseEnclosed parser =
    let enclosed (count, tokens) =
        match tokens with
        | [] -> ParserEndOfStream LPAR
        | (LPAR, _) :: tokens ->
            match Parse parser (count, tokens) with
            | Error msg -> Error msg
            | Ok (regex, (count, tokens)) ->
                match tokens with
                | [] -> ParserEndOfStream RPAR
                | (RPAR, _) :: tokens ->
                    (regex, (count, tokens))
                    |> Ok

                | (token, pos) :: _ ->
                    ParserError RPAR token pos

        | (token, pos) :: _ -> ParserError LPAR token pos

    Parser enclosed
          

let ParseStar parser =
    let Star input =
        match Parse parser input with
        | Error msg -> Error msg
        | Ok (regex, (count, tokens)) ->
            match tokens with
            | [] -> ParserEndOfStream STAR
            | (STAR, pos) :: tokens ->
                let star = 
                    (regex, FPos regex, LPos regex)
                    |> Star

                (star, (count, tokens))
                |> Ok

            | (token, pos) :: tokens ->
                ParserError STAR token pos

    Parser Star

let ParsePlus parser =
    let Plus input =
        match Parse parser input with
        | Error msg -> Error msg
        | Ok (regex, (count, tokens)) ->
            match tokens with
            | [] -> ParserEndOfStream PLUS
            | (PLUS, _) :: tokens ->
                let count, regex =
                    let star = // create star node
                        (regex, FPos regex, LPos regex)
                        |> Star
                    // fixup state numbering in regex
                    let count, regex = FixupState count regex

                    // adding the fixed regex in front of star to adhire to the plus pattern.
                    count, Cat (regex, star, CatFirstPos regex star, CatLastPos regex star, CatNullAble regex star)
                
                Ok (regex, (count, tokens))
        
            | (token, pos) :: _ ->
                ParserError PLUS token pos
    
    Parser Plus


let ParserInner =
    let rec inner acc (count, tokens) =
        match tokens with
        | [] -> ParserEndOfStream "any atom"
        | (ATOM (a : char), _) :: (LINE, _) :: (ATOM b, _) :: tokens ->
            let lst = 
                if a < b then
                    [a .. b]
                else
                    [b .. a]
            inner (List.fold (fun acc x -> x :: acc) acc lst) (count, tokens)
        
        | (ATOM a, _) :: tokens -> 
            inner (a :: acc) (count, tokens)

        | (RBRA,_) :: _ ->
            Ok (acc, (count, tokens)) // doublicate are onlikely, but will be removed in the DFA minimization algorithm. 

        | (token, pos) :: _ -> ParserError "any atom" token pos

    Parser (inner [])

let rec ParserMakeInterval pos acc count =
    function
    | [] -> count, acc
    | x :: xs -> 
        ParserMakeInterval pos ((Atom (x, count, pos)) :: acc) (count + 1) xs
    
    


let ParseInteral =
    let interval (count, tokens) =
        match tokens with
        | [] -> ParserEndOfStream LBRA
        | (LBRA, _) :: tokens ->
            match Parse ParserInner (count, tokens) with
            | Error msg -> Error msg
            | Ok (lst, (count, tokens)) ->
                match tokens with
                | [] -> ParserEndOfStream RBRA
                | (RBRA, pos) :: tokens ->
                    let count, reglst = ParserMakeInterval pos [] count lst
                    let regex = List.fold (fun regex reg -> Or (regex, reg, OrFirstPos regex reg, OrLastPos regex reg, false)) reglst.Head reglst.Tail
                    Ok (regex, (count, tokens))
                
                | (token, pos) :: tokens -> 
                    ParserError RBRA token pos

        | (token, pos) :: _ ->
            ParserError LBRA token pos

    Parser interval



let ParseComplement =
    let interval (count, tokens) =
        match tokens with
        | [] -> ParserEndOfStream LBRA
        | (LBRA, _) :: tokens ->
            match Parse ParserInner (count, tokens) with
            | Error msg -> Error msg
            | Ok (lst, (count, tokens)) ->
                match tokens with
                | [] -> ParserEndOfStream RBRA
                | (RBRA, pos) :: tokens ->
                    let count, reglst = // find the complement set of symbols of lst, and increment count accordently
                        asciiprintable
                        |> List.filter (fun x -> not <| List.contains x lst)
                        |> ParserMakeInterval pos [] count 

                    let regex = List.fold (fun regex reg -> Or (regex, reg, OrFirstPos regex reg, OrLastPos regex reg, false)) reglst.Head reglst.Tail
                    Ok (regex, (count, tokens))
                
                | (token, pos) :: tokens -> 
                    ParserError RBRA token pos

        | (token, pos) :: _ ->
            ParserError LBRA token pos

    Parser interval


// simple units of 
let RegexSymbolNestedIntervalOrComplement parser =
    ParseAtom <?> ParseEnclosed parser <?> ParseInteral <?> ParseComplement

let RegexStarOrPlusOrSimple parser =
    ParseStar parser <?> ParsePlus parser <?> RegexSymbolNestedIntervalOrComplement parser

let POr regex1 regex2 =
    Or (regex1, regex2, OrFirstPos regex1 regex2, OrLastPos regex1 regex2, OrNullAble regex1 regex2)

let PCat regex1 regex2 =
    Cat (regex1, regex2, CatFirstPos regex1 regex2, CatLastPos regex1 regex2, CatNullAble regex1 regex2)

let RegexOrCat parser =
    let roc input =
        match Parse (RegexStarOrPlusOrSimple parser) input with
        | Error msg -> Error msg
        | Ok (regex1, (count, tokens)) ->
            match tokens with
            | [] ->  Ok (regex1, (count, tokens)) // end of stream
            | (OR, _) :: tokens ->
                match Parse (RegexStarOrPlusOrSimple parser) (count, tokens) with
                | Error msg -> Error msg
                | Ok (regex2, (count, tokens)) ->
                    Ok (POr regex1 regex2, (count, tokens))

            | _ -> 
                // Try parse as cat will cathc all other errors
                match Parse (RegexStarOrPlusOrSimple parser) (count, tokens) with
                | Error msg -> Error msg
                | Ok (regex2, (count,tokens)) ->
                    Ok (PCat regex1 regex2, (count, tokens))

    Parser roc


let NextRegex =
    let rec parse (count, tokens) =
        match Parse (RegexOrCat (Parser parse)) (count, tokens) with
        | Ok next -> Ok next
        | Error _ ->
            match Parse (RegexStarOrPlusOrSimple (Parser parse)) (count, tokens) with
            | Error msg -> Error msg
            | Ok next -> Ok next
    Parser parse

let ParseRegex lexer =
    let rec parser regex (count, tokens) =
        match tokens with
        | [] -> Ok (regex, (count,[]))
        | (OR, _) :: tokens ->
            match Parse NextRegex (count, tokens) with
            | Error msg -> Error msg
            | Ok (regex', rest) ->
                parser (POr regex regex') rest

        | _ ->
            match Parse NextRegex (count, tokens) with
            | Error msg -> Error msg  
            | Ok (regex', rest) ->
                parser (PCat regex regex') rest

    let p input =
        match Parse NextRegex input with
        | Error msg -> Error msg
        | Ok (regex, rest) ->
            parser regex rest

    let all input =
        match Lex lexer input with
        | Error msg -> Error msg
        | Ok (tokens, _) ->
            p (0, tokens)

    Parser (fun lst -> all (lst, startPos))



