(* Sorting a list of lists according to length of sublists.

We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
*)

let length_sort l = 
    let len_comp a b = List.length a - List.length b in
    List.sort len_comp l

module IntMap = Map.Make(Int)

let create_freq l =
    let touch t s =
        let len = List.length s in
        let count =
            match IntMap.find t len with
            | None -> 0
            | Some n -> n
        in
        IntMap.add t ~key:len ~data:(count + 1)
    in
    List.fold l ~init:IntMap.empty ~f:touch

let frequency_sort l =
    let freq = create_freq l in
    let freq_comp a b = 
        let find n = 
            match IntMap.find freq n with
                | None -> 0
                | Some v -> v
        in
        find (List.length a) - find (List.length b) in
    List.sort freq_comp l
