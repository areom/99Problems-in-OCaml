(* Calculate Euler's totient function φ(m) (improved). *)
let phi_improved n =
    let rec pow n p = if p < 1 then 1 else n * pow n (p - 1) in
    let factors = 
        let rec aux d n =
            if n = 1 then [] else
                if n mod d = 0 then
                    match aux d (n / d) with
                    | (div, cnt) :: t when div = d -> (d, cnt+1) :: t
                    | l -> (d, 1) :: l
                else aux (d+1) n
        in 
        aux 2 n
   in
   List.fold factors ~init:1 ~f:(fun init (p, m) -> init * (p - 1) * pow p (m - 1))
