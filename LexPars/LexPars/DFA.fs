module DFA

open SymbolTable
open Regex

// this is another implementation of Transistion mapping
// it's makes it possible to combine several lexing/parsing algorithms into one.
type DFA<'input, 'output> = DFA of ('input -> Result<'output, string>) 

let Transition (DFA dfa) = dfa 

type DFAConstructionData<'R when 'R : comparison> =
    {
        symbols : 'R Set
        startState : int Set
        followpos : Map<int, int Set>
        transitionOn : Map<char, int Set>

    }

let GetTransitions regex =
    let rec gather transitions =
        function
        | Atom (a, n, _) -> 
            match Map.tryFind a transitions with
            | None -> Map.add a (Set.singleton n) transitions
            | Some s ->
                let s = Set.add n s
                Map.add a s transitions
        | Terminal _ ->
            transitions
        
        | Star (reg, _, _) ->
            gather transitions reg

        | Cat (reg1,reg2, _, _, _) | Or (reg1,reg2, _, _, _) ->
            let transitions = gather transitions reg1
            gather transitions reg2
    
    gather Map.empty regex
  

let GetLanguage regex =
    let rec gather language =
        function
        | Atom (a, _, _) ->
            Set.add a language
        | Terminal _ -> language
        | Star (reg,_, _) ->
            gather language reg
        | Or (reg1, reg2, _, _, _) 
        | Cat (reg1, reg2, _, _, _) ->
            let language = gather language reg1
            gather language reg2
    
    gather Set.empty regex



let GetData regex =
    {
        symbols = GetLanguage regex // get all symbols of the language
        startState = FPos regex // get starting state
        followpos = FollowPos Map.empty regex // get followpos'
        transitionOn = GetTransitions regex // get 
    }



// find U given S, where ps are the positions where some predefined symbol a exists
let findU S ps data =
    // find common positions p of ps and S
    Set.intersect S ps
    // union the followpos of all p's 
    |> Set.fold (fun fp p -> Set.union fp <| (data.followpos.TryFind p).Value) Set.empty  



let makeDFA regex =
    let data = GetData regex

    let rec gather transitions unmarked marked =
        match Set.isEmpty unmarked with
        | true -> marked, transitions
        | _ -> 
            // find all Us for all state S in unmarked states
            let UsOfS = 
                Set.fold (fun lst S ->
                    // return all U's with symbol
                    let Us = 
                        Map.toList data.transitionOn
                        |> List.map (fun (symbol, ps) -> (symbol, findU S ps data))
                        
                    (S, Us) :: lst
                ) [] unmarked
            
            // the states of new states 
            let newStates =
                List.fold (fun states (_, Us) ->
                        List.fold 
                            ( fun us (_, u) ->
                                Set.add u us
                            ) states Us
                    ) Set.empty UsOfS
           
            // add all old unmarked to marked states
            let marked = Set.union marked unmarked
            // get all new states as unmarked filtering old unmarkd and
            // unmarked will always converge to the empty set,
            // since the language are finite
            let unmarked = Set.difference newStates marked

            let transitions =
                List.map(fun (S, Us) ->
                        List.map (fun (symbol, U) -> (symbol,S,U) ) Us
                   ) UsOfS
                   |> List.concat
                   |> Set.ofList
                   |> Set.union transitions
                

            gather transitions unmarked marked
    
    gather Set.empty (Set.singleton data.startState) Set.empty
    (*
        OBS the return value aboe arn't the right one. just an intermedia calculation
        it return the tuple of the set of all states and the set of transitions

        TODO:
         - add terminal markes and find terminal states
         - replace set of positions representation of states with numbers
         - to place the startstate first remove it from the set
           make the new set to a list cons startstate in front of it.

        - create an array of size numberofstates * (languagesize + 1)
          alter entries to the corresponding jump or terminations.
          an entry either return a result type or a recursive call 
          to the outer function.

        - return the outer function with start state set.
    *)

                