(* Split a list into two parts; the length of the first part is given. *)
let split l n =
    let rec create_2_lists first second len = function
        | [] -> ((List.rev first), (List.rev second))
        | hd::tl -> if len > n then create_2_lists first (hd::second) len tl
                               else create_2_lists (hd::first) second (len+1) tl
    in
    create_2_lists [] [] 1 l
