(* Rotate a list N places to the left. *)
let rotate l n = 
    let split l n =
        let rec create_2_lists first second len = function
            | [] -> ((List.rev first), (List.rev second))
            | hd::tl -> if len > n then create_2_lists first (hd::second) len tl
                                  else create_2_lists (hd::first) second (len+1) tl
        in
    create_2_lists [] [] 1 l
    in

    let split_point = ((List.length l) + n) mod (List.length l) in
    let h, t = split l split_point in
    t @ h
