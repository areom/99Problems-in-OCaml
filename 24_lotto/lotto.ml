(* Lotto: Draw N different random numbers from the set 1..M *)
let range a b =
    let rec aux low hi =
        if low < hi then low::(aux (low + 1) hi)
                     else [hi]
    in
    if a < b then aux a b else List.rev (aux b a)

let rec rand_select l n = 
    if n > 0 then (List.nth l (Random.int (List.length l))) :: (rand_select l (n-1))
                else []

let lotto m n = rand_select (range 1 m) n
