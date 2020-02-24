module SymbolTable
    

    (*
        two implementation of the same data structure
    *)


    // fastes for big symbol table and should be used in general
    type SymbolMap<'key,'item when 'key : comparison> = SMap of Map<'key,'item>
    with

        static member Lookup key (SMap table) =
            match Map.tryFind key table with
            | None -> Error <| sprintf "The key " + string key + " was not found!"
            | Some item -> 
                Ok item

        static member Bind key item (SMap table) =
            Map.add key item table
            |> SMap
            
        static member Remove key (SMap table) =
            Map.remove key table
            |> SMap

        static member Map f (SMap table) =
            Map.map f table
            |> SMap

        static member Fold f acc (SMap table) =
            Map.fold f acc table

        static member Union sm1 sm2 =
            SymbolMap<'key,'item>.Fold (fun table key item -> SymbolMap<'key,'item>.Bind key item table) sm2 sm1

    
    // for intermidia use with many updates this will do find
    type SymbolList<'key, 'item> = SList of ('key * 'item) list
    with
        
        static member Bind key item (SList lst) =
            (key,item) :: lst
            |> SList

        static member Lookup key (SList lst) =
            match List.tryFind key lst with
            | None -> Error <| sprintf "The key " + string key + " was not found!"
            | Some item -> Ok item

        // will remove all bindings of key
        static member Remove key (SList lst) =
            List.filter (fun pair -> key <> fst pair) lst
            |> SList

        static member Map f (SList lst) =
            List.map f lst
            |> SList

        static member Fold f acc (SList lst) =
            List.fold f acc lst

        static member Union lst1 lst2 =
            SymbolList<'key,'item>.Fold (fun lst (key, item) -> SymbolList<'key,'item>.Bind key item lst) lst2 lst1
            

