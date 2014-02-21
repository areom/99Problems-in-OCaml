(* Create a list containing all integers within a given range. *)
let range a b =
    let rec aux low hi =
        if low < hi then low::(aux (low + 1) hi)
                     else [hi]
    in
    if a < b then aux a b else List.rev (aux b a)
