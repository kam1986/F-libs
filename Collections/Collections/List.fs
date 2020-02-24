module List

open System.Threading.Tasks

    
(*
    Persistent single linked list.
        A list is simply a sequence of functions. 
        
        Even though this are quit fast it is not meant for usage, just a mental exercise
        in a uncommen way of thinking of data structures and the power of functions seen as data.
        
    OBS : stack based list and memory usage are biggere than those of none function based structure.
          
*)

// This already exist in F# I just like the naming of this better.
// needed to handle error cases better with error handling

// heap based 
type Result<'success,'error> =
    | Result of ret : 'success
    | Error of err : 'error

type Action =
    | Next 
    
// hide implementation specifics by using a function as a list
// OBS : the underlying representation of the list are irrelevant as long as the implementation adhire to
// the expected list functions behavior
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

(*
   Appnd the two list in O(k) time, by delaying iterations over lst2 until lst1 is empty.
   this increase the worst case cost of iteration over an abitrary list composition.
   the Head function still runs in O(k) time since we always have that the head of the list 
   are at top most level
*)
(*
    OBS : Fold, FoldBack, Map, and Filter all remove concatenation in the list, hence decrease later iterations time.
*)


let rec Fold f acc lst =
    match Next lst with
    | Error _ -> acc
    | Result (head, tail) -> Fold f (f acc head) tail

// ineffecient do to massive none tail recursive calls
let rec FoldBack f lst acc =
    match Next lst with
    | Error _ -> acc
    | Result (head, tail) -> f head (FoldBack f tail acc) 

let Rev lst =
    Fold (fun acc a -> Cons a acc) Empty lst

let Append lst1 lst2 =
    FoldBack (fun a acc -> Cons a acc) lst1 lst2
(*
    Need to implement Scan and ScanBack
*)
        

// properly more efficient with a direct implementation
let Map f lst =
    Fold (fun acc head -> Cons (f head) acc) Empty lst
    |> Rev

let Filter pred lst =
    FoldBack (fun x xs -> if pred x then Cons x xs else xs) lst Empty

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
    Merge sort implementation
    Iterations:
        expand 'a List to 'a List List of singletones i.e. singletons are always ordered.
        loop :
            merge pairwise each list in the 'a list list into a sorted 'a list
            if 'a list list has only one list left it returns that list
            else repeat
*)

let rec private sort acc lst1 lst2 =
    match Next lst1, Next lst2 with
    | Error _, _ -> 
        Fold (fun acc x -> Cons x acc) acc lst2
        |> Rev
        
    | _, Error _ -> 
        sort acc Empty lst1
        
    | Result (head1, tail), Result (head2, _) when head1 < head2 ->
        sort (Cons head1 acc) tail lst2

    | Result _, Result (head, tail) ->
        sort (Cons head acc) lst1 tail

let rec private sortAll acc lst =
    match Next lst with
    | Result (head, tail) ->
        match Next tail with
        | Result (head', tail') ->
            // this ships of the sorting task to a job queue
            let task = (new Task<'a List>(fun _ -> sort Empty head head'))
            task.Start() // run task in parallel
            let acc' = Cons task acc
            sortAll acc' tail'
    
        | Error _ -> 
            // fetch results from tasks
            Map (fun (t : Task<'a List>) -> t.Result) acc 
            |> Cons head

    // fetch result from tasks.
    | Error _ -> Map (fun (t : Task<'a List>) -> t.Result) acc
                
// stack overflow on big list do to the List representation as functions
let rec MSort lst  =
    let rec merge llst =
        let lst = sortAll Empty llst
        match Next <| Tail lst with // empty tail if error
        | Error _ ->
            match Head lst with
            | Error _ -> Empty
            | Result head -> head

        | _ -> merge lst
                
                

    // return value
    Map (fun x -> Cons x Empty) lst // expand to 'a list list of singletone lists
    // merging and sorting the inner list recursively 
    |> merge   


let ofList lst =
    List.foldBack (fun a acc -> Cons a acc) lst Empty

let toList lst =
    FoldBack (fun a acc -> a :: acc) lst []


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