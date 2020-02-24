module Mapping
    
(*

    This is a general implemtation of a function type
    It is used in both lexergenerator and parsergenerator creation makes it easy to handle error message parsing, without interrups
    The Leaf functions called Expect are not given her since they are implementation specific

*)


// General function type with lightweight error handling
type Mapping<'input,'output,'error> = Mapping of ('input -> Result<'output * 'input, 'error>)

// Run the transformatin map
let Run (Mapping map) = map

let Result ret =
    match ret with
    | Error msg -> 
        printf "%s" msg
        exit -1 // interrup the program process
    | Ok (output, _) -> output


// general binding function
let Bind output = 
    let ret input =
        match input with
        | _ -> Ok (output, input)
    Mapping ret


let Or map1 map2 =
    let o input =
        match Run map1 input with
        | Ok ret -> Ok ret
        | _ ->
            match Run map2 input with
            | Ok ret -> Ok ret
            | Error msg -> Error msg
    Mapping o

let ( <|> ) = Or

// 
let Choose mlist =
    List.reduce Or mlist


let And map1 map2 =
    let a input =
        match Run map1 input with
        | Error msg -> Error msg
        | Ok (output1, rest) ->
            match Run map2 rest with
            | Error msg -> Error msg
            | Ok (output2, rest') -> Ok ((output1, output2), rest')
    Mapping a

let ( <&> ) = And


let Map f map =
    let map input =
        match Run map input with
        | Error msg -> Error msg
        | Ok (output, rest) -> Ok(f output, rest)
    Mapping map

let Filter p map =
    let rec filter input =
        match Run map input with
        | Ok (output, rest) when p output -> Ok (output, rest)
        | Ok (_, rest) -> filter rest
        | Error msg -> Error msg
    Mapping filter


let Apply funcAsMap paramAsMap =
    funcAsMap <&> paramAsMap
    |> Map (fun (f, output) -> f output)

let ( => ) = Apply

let Lift func paramAsMap1 paramAsMap2 =
    Bind func => paramAsMap1 => paramAsMap2

// a little odd named since it isn't the true form of a fold for a mapping
let Fold func acc mlst =
    let funcAsLift = Lift func
    List.foldBack (fun map acc  -> funcAsLift map acc) mlst acc


let Sequenize mlst =
    let cons h t = h :: t
    Fold cons (Bind []) mlst  // uses list because they are faster then sequences and the output of this are nearly never large.