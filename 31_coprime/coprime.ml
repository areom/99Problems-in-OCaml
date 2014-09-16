(* Determine whether two positive integer numbers are coprime. *)
let coprime a b =
    let rec gcd a b =
        if b = 0 then a else gcd b (a mod b)
    in
    (gcd a b) = 1
