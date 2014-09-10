(* Goldbach's conjecture. *)
let is_prime n =
    let n = abs n in
    let rec divisible k =
        match n mod k with
        | 0 when n <> 2 -> true
        | _ -> if k * k > n then false
               else divisible (k+1)
    in
    not (divisible 2)

let goldbach n =
    let rec tryout d =
        if is_prime d && is_prime (n - d) then (d, n-d) else tryout (d+1)    
    in
    tryout 2
