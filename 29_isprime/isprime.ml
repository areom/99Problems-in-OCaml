(* Determine whether a given integer number is prime. *)
(* Naive solution *)
let is_prime n =
    let n = abs n in
    let rec divisible k =
        match n mod k with
        | 0 when n <> 2 -> true
        | _ -> if k * k > n then false
               else divisible (k+1)
    in
    not (divisible 2)
