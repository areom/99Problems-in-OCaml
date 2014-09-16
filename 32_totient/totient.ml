(* Calculate Euler's totient function φ(m) *)
let phi n =
    let coprime a b =
        let rec gcd a b =
            if b = 0 then a else gcd b (a mod b)
        in
        (gcd a b) = 1
    in
    let rec aux i =
        let incre = if coprime i n then 1 else 0 in
        if i = (n-1) then incre else (aux (i+1)) + incre
    in
    aux 1
