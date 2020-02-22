module List
(*
    persistent single linked list, with constant cons and Appending.
    OBS : memory usage are biggere than those of none function based, but the different of the List structure designed by Okasaki is that
    this design can be measured in worst case too.

    A keen observer would see that clever usage of Append can lead to an implementation of unary search trees.

    Assuming that an iteration over Cons has cost c1 and Append has c2 cost, we then have that the total cost of any list takes
        c1 * k + c2 * (n-k) = (c1 - c2) * k + c2 * n, where k is the number of Cons operation done on that list up until the moment of iteration.
        We see that if c1 < c2 and k = 0 then the worst case would be c2 * n, and WLG we can say the same for c1 > c2.
*)

// This already exist in F# I just like the naming of this better.
// needed to handle error cases better with error handling

// heap based 
type Result<'success,'error> =
    | Result of ret : 'success
    | Error of err : 'error

type Action =
    | Next 
    
// hide implementation specifiks
// the inner representation can be any collection structure adhiring to the list commands
type 'L List = List of (Action -> Result<'L * 'L List,string>) 

let Next (List lst) =
    lst Next

let Empty  =
    List (fun _ -> Error "Empty")

let Cons a lst =
    let lst =
        (fun Next -> 
            Result (a, lst) 
        )
    List lst

let Head lst =
    match Next lst with
    | Error msg -> Error msg
    | Result (head, _) -> Result head
    
// this adhire to the normal behavior namely that the tail of an empty list is the empty list
let Tail lst =
    match Next lst with
    | Result (_, tail) -> tail
    | _ -> Empty

let Length lst =
    let rec len count lst =
        match Next lst with
        | Error _ -> count
        | Result (_, tail) -> len (count + 1) tail

    len 0 lst

// Appnd the two list by delaying iterations over lst2 until lst1 is empty.
// this increase the worst case cost of iteration over an abitrary list composition.
// but this is neglegent compared to the alternatives.
let rec Append (List lst1) (List lst2 as lst) =
    match Next (List lst1) with
    | Error _ -> List lst2 // eleminates concatination sequences of empty lists 
    | Result _ ->
        let list action =
            match lst1 action with
            | Result (a, tail) -> Result (a, Append tail lst)
            | Error _ ->
                lst2 action

        List list


(*
    OBS : Fold, FoldBack, Map, and Filter all remove concatenation in the list, hence increase later iterations time.
*)


let rec Fold f acc lst =
    match Next lst with
    | Error _ -> acc
    | Result (head, tail) -> Fold f (f acc head) tail

// ineffecient do to massive none tail recursive calls
let rec FoldBack f lst acc =
    match Next lst with
    | Error _ -> acc
    | Result (head, tail) -> f (FoldBack f tail acc) head

let Rev lst =
    Fold (fun acc a -> Cons a acc) Empty lst

(*
    Need to implement Scan and ScanBack
*)
        

// properly more efficient with a direct implementation
let Map f lst =
    FoldBack (fun acc head -> Cons (f head) acc) lst Empty

let Filter pred lst =
    FoldBack (fun xs x -> if pred x then Cons x xs else xs) lst Empty

// handle error case with option type.
let Reduce f lst =
    match Next lst with
    | Error msg -> None
    | Result (head, tail) ->
        Fold f head tail
        |> Some

let Concat lst =
    FoldBack Append lst Empty


(* 
    Merge sort
    Iterations:
        expand 'a List to 'a List List of singletones i.e. singletons are always ordered.
        loop :
            merge pairwise each 'a list in the 'a list list into a sorted 'a list
            if 'a list list has only one list left it returns that list
            else repeat
*)
let rec MSort lst =
    let rec sort lst1 lst2 =
        match Next lst1, Next lst2 with
        | Error _, _ -> lst2
        | _, Error _ -> sort Empty lst1
        | Result (head1, tail), Result (head2, _) when head1 < head2 ->
            Cons head1 <| sort tail lst2
        | Result _, Result (head, tail) ->
            Cons head <| sort lst1 tail

    let rec merge acc llst =
        match Next llst with
        | Error _ -> 
            match Next <| Tail acc with 
            | Error _ -> // convergence point
                match Head acc with
                | Error _ -> Empty
                | Result lst -> lst
            | _ -> merge Empty acc // repeat

        | Result (head1, tail) ->
            match Next tail with
            | Error _ -> // case of uneven number of list to merge
                Cons head1 acc 
                |> merge Empty

            | Result (head2, tail2) ->
                merge (Cons (sort head1 head2) acc) tail2


    Map (fun x -> Cons x Empty) lst // expand to 'a list list of singletone lists
    |> merge Empty    
        
    

                


let ofList lst =
    List.foldBack (fun a acc -> Cons a acc) lst Empty

let toList lst =
    FoldBack (fun acc a -> a :: acc) lst []


// input a list of F# list as test cases
// Does not test the ordering difference of Fold and FoldBack
let WhiteBoxTesting testbases = 
    List.fold
        (fun _ testbase ->
            printfn "Test subject : %A" testbase
            let subject = List.foldBack Cons testbase Empty
            // list of tuples paranteses and element seperator are omitted 
            [ 
                "Testing Cons operator and toList", toList subject = testbase
                "Testing Filter", (toList << Filter (fun x -> x < 6)) subject = List.filter (fun x -> x < 6) testbase 
                "Testing Map", (toList << Map (fun x -> x + 1)) subject = List.map (fun x -> x + 1) testbase
                "Testing Fold", Fold (fun sum a -> a + sum) 0 subject = List.fold (fun sum a -> a + sum) 0 testbase
                "Testing Rev", (toList << Rev) subject = List.rev testbase
                "Testing FoldBack", Fold (fun sum a -> a + sum) 0 subject = List.foldBack (fun a sum -> a + sum) testbase 0
                "Testing Append", (toList << Append subject) subject = List.append testbase testbase
                "Testing Concat", (toList << Concat) (Cons subject <| Cons subject Empty) = List.concat [testbase; testbase]
            ]
            |> List.fold 
                (fun _ (test, result) -> 
                    if result then 
                        printfn "%s: Success!" test
                    else
                        printfn "%s: Failure!" test 
                        exit(-1) // force termination on faulty testing.
                ) () // accumulator irrelevant hence wildcard
            printfn "" // newline seperator
        ) () testbases 


// testing only corner case and normal list
[[];[1..10]]
|> WhiteBoxTesting