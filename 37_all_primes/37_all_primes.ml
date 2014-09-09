(* A list of prime numbers. *)
let rec all_primes low high = 
    let is_prime n =
        let n = abs n in
        let rec divisible d =
            match n mod d with
            | 0 when n <> 2 -> true
            | _ -> if d * d > n then false
                                  else divisible (d+1)
        in
        not (divisible 2)
    in
    if low > high then []
        else if is_prime low then low :: all_primes (low + 1) high
            else all_primes (low + 1) high
