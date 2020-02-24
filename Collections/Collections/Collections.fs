
module Collection
    
    module Simple =
    
        let private erremps = Error "Empty stack"
        let private errempq = Error "Empty queue"
        let private errempl = Error "Empty list"

        // 
        type 's stack = Stack of 's list
        with
            static member Peek (Stack lst) =
                match lst with
                | [] -> erremps
                | x::xs ->
                    Ok (x, Stack lst)

            static member Pop (Stack lst) =
                match lst with
                | [] -> erremps
                | x::xs ->
                    Ok (x, Stack xs)

            static member Push value (Stack lst) =
                value :: lst
                |> Stack

            static member PopMany n (Stack lst) =
                let rec popper n acc lst = 
                    if n = 0 then
                        Ok (acc, Stack lst)
                    else
                        match lst with
                        | [] -> erremps
                        | x :: xs -> popper (n-1) (x :: acc) xs
                
                popper n [] lst

            static member PushMany values (Stack lst) =
                List.fold (fun stack value -> value :: stack) lst values

            static member map f (Stack lst) =
                List.map f lst
                |> Stack

            static member filter pred (Stack lst) =
                List.filter pred lst
                |> Stack

            static member fold f acc (Stack lst) =
                List.fold f acc lst

            static member foldBack f (Stack lst) acc =
                List.foldBack f lst acc

            static member Reduce f (Stack lst) =
                List.reduce f lst

            static member inline Empty() = Stack []

        type 'Q queue = Queue of 'Q list * 'Q list
        with
            static member inline Empty() = Queue (([] : 'a list),([] : 'a list))

            static member Enqueue value (Queue (front, back)) =
                (front, value :: back)
                |> Queue

            static member Dequeue (Queue (front, back)) =
                match front with
                | x :: xs -> 
                    Ok (x, Queue(xs, back))

                | [] ->
                    match back with
                    | [] -> errempq
                    | _ ->
                        let front' = List.rev back
                        match front with
                        | [] -> errempl
                        | head::front' ->
                        Ok (head, Queue (front', []))

            // map over both list do not reverse and append the back list to the front list
            static member map f (Queue (front, back)) =
                Queue (List.map f front, List.map f back)

            
            static member filter pred (Queue (front, back)) =
                Queue (List.filter pred front, List.filter pred back)


            static member fold f acc (Queue (front, back)) =
                // since the back lst are in reverse we need to run it backward after fold on front
                List.fold (fun acc x -> f acc x) acc front
                |> List.foldBack (fun x acc -> f acc x) back // folding over the output of the line above


            static member foldBack f (Queue (front, back)) acc =
                // same as with fold just in rerverse
                List.fold (fun acc x -> f acc x) acc back
                |> List.foldBack (fun x acc -> f acc x) front // folding over the output of the line above

        // List with constant Append operation
        type 'A AppList =
            | Empty
            | AList of 'A list * 'A AppList queue // the 'A list queue simple enqueue the list to be appended.
        with
            static member Cons value lst =
                match lst with
                | Empty -> AList (value::[], queue.Empty())
                | AList (front, back) -> AList (value::front, back) 
               
            static member ( <+> ) (a, b) = AppList.Cons a b



            static member Append lst1 lst2 =
                match lst1 with
                | Empty -> lst2
                | AList(front, back) ->
                   let back' = queue.Enqueue lst2 back
                   AList (front, back')

            static member map f lst =
                match lst with
                | Empty -> lst
                | AList (front, back) ->
                    let back' = queue.map (AppList.map f) back
                    AList (List.map f front, back')
                    
            static member filter pred lst =
                match lst with
                | Empty -> lst
                | AList(front, back) ->
                    let back' = queue.map (AppList.filter pred) back
                    let front' = List.filter pred front
                    AList (front', back')

            static member fold f acc lst =
                match lst with
                | Empty -> acc
                | AList(front, back) ->
                    let acc' = List.fold f acc front
                    queue.fold (AppList.fold f) acc' back

            // ineffecient implementation will be rewriten at a later date, have som problem with type mismatch
            static member foldBack f lst acc =
                match lst with
                | Empty -> acc
                | AList(front, back) ->
                    let lst = 
                        AppList.fold (fun xs x -> x :: xs) [] lst
                        |> List.rev
                    List.foldBack f lst acc
                    
                    


            // to antissipate the case where we have and empty front and none empty back this implemented insted of head and tail functions seperately.
            // this will when front are empty collaps it to a normal list and insert it as the front of a new AList
            // this will run in O(n) worst time and O(k) atormized time.
            // Assume that we have a Alist of appended Alist with all empty front list, with a total of N elements.
            //
            // The first next will then take k * N where k is the constant cost of Cons for each element.
            // The next N-1 next will then take k cost, hence k * N + k * (N - 1) = k * (2 * N - 1) < 2 * k * N
            //
            // We then have that the armotized cost are 2 * k * N / N = 2 * k = O(k)
            static member next lst =
                match lst with
                | Empty -> errempl
                | AList (front, back) ->
                    match front with
                    | head :: rest ->
                       Ok (head, AList (rest, back))
                    
                    | [] -> // collapsing.
                       let lst = queue.foldBack (AppList.foldBack List.Cons) back []
                       lst 



            (*
            
                Next to be implemented are two different kind of Fibonacci heaps, one with normal data structures, one over a memory mapped file

            *)



                    




    
    

