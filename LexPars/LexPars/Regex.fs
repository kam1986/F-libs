module Regex

open Position


type 'r regex =
    | Terminal of int | Atom of 'r * int * position
    | Star of 'r regex * int Set * int Set 
    | Or of 'r regex * 'r regex * int Set * int Set * bool 
    | Cat of 'r regex * 'r regex * int Set * int Set * bool

// nullable, first, last and followpos Cattruction algorithms
let FPos =
    function
    | Terminal n | Atom (_, n, _) -> set[n]
    | Star (_, fp, _) -> fp
    | Or   (_, _, fp, _, _) -> fp
    | Cat (_, _, fp, _, _) -> fp

let LPos =
    function
    | Terminal n | Atom (_, n, _) -> set[n]
    | Star (_, lp, _) -> lp
    | Or   (_, _, lp, _, _) -> lp
    | Cat (_, _, _, lp, _) -> lp

let NullAble =
    function
    | Terminal _ | Atom _ -> false
    | Star _ -> true
    | Or (_, _, _, _, n) -> n
    | Cat (_, _, _, _, n) -> n

let OrFirstPos reg1 reg2 = Set.union <| FPos reg1 <| FPos reg2

let OrLastPos reg1 reg2 = Set.union <| LPos reg1 <| LPos reg2

let OrNullAble reg1 reg2 = NullAble reg1 || NullAble reg2

let CatLastPos reg1 reg2 =
    match NullAble reg2 with
    | false -> LPos reg2
    | _ -> Set.union <| LPos reg1 <| LPos reg2

let CatFirstPos reg1 reg2 =
    match NullAble reg2 with
    | false -> FPos reg1
    | _ -> Set.union <| FPos reg1 <| FPos reg2

let CatNullAble reg1 reg2 = NullAble reg1 && NullAble reg2


let AddFollowpos i fpos followpos =
    match Map.tryFind i followpos with
    | Some follow -> 
        Map.add i <| Set.union follow fpos <| followpos 
    | None ->
        Map.add i fpos followpos


let rec FollowPos map =
    function
    | Atom _ | Terminal _ ->
        map

    | Or (reg1, reg2, _, _, _) ->
        let map' = FollowPos map reg1
        FollowPos map' reg2

    | Cat (reg1, reg2, _, _, _) ->
        let map' = FollowPos map reg1
        let map'' = FollowPos map' reg2
        Set.fold (fun map i -> AddFollowpos i (FPos reg2) map) map'' (LPos reg1)
        
    | Star (reg, fp, lp) ->
        let map' = FollowPos map reg
        Set.fold (fun map i -> AddFollowpos i fp map) map' lp


// fixup statenumbering e.i. number in atom and terminals
// helpfull when expanding regular expressions from extended version to normal form
let rec FixupState count =
    function
    | Atom (a, n, pos) -> count + 1, Atom (a, count, pos)
    | Terminal n -> count + 1, Terminal count
    | Star (regex, fp, lp) ->
        FixupState count regex
    
    | Cat (regex1, regex2, _, _, n) ->
        let count, regex1 = FixupState count regex1
        let count, regex2 = FixupState count regex2
        count, Cat (regex1, regex2, CatFirstPos regex1 regex2, CatLastPos regex1 regex2, n)

    | Or (regex1, regex2, _, _, n) ->
        let count, regex1 = FixupState count regex1
        let count, regex2 = FixupState count regex2
        count, Or (regex1, regex2, OrFirstPos regex1 regex2, OrLastPos regex1 regex2, n)
