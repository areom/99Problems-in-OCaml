(* Determine whether a given integer number is prime. *)
(* Naive solution *)
let is_prime n =
    let n = abs n in
    let rec divisible k =
        match n mod k with
        | 0 -> false
        | _ -> if (n / 2) = k then true
               else divisible (k+1)
    in
    divisible 2
