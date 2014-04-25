(* Extract a given number of randomly selected elements from a list *)
let rec rand_select l n = 
    if n > 0 then (List.nth l (Random.int (List.length l))) :: (rand_select l (n-1))
                else []
