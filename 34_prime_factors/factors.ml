(* Determine the prime factors of a given positive integer (2) (encoded) *)
let factors n = 
    let rec aux d n =
        if n = 1 then [] else
            if n mod d = 0 then
                match aux d (n / d) with
                | (div, cnt) :: t when div = d -> (d, cnt+1) :: t
                | l -> (d, 1) :: l
            else aux (d+1) n
    in 
    aux 2 n
