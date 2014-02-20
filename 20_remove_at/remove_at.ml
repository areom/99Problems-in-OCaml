(* Remove the K'th element from a list. *)
let rec remove_at i = function
    | [] -> []
    | h::t -> if i = 0 then t
                       else h::(remove_at (i - 1) t)

let remove_at_tr i l =
    let rec aux i first = function
        | [] -> List.rev first
        | hd::tl -> if i = 0 then (List.rev first) @ tl
                             else aux (i - 1) (hd :: first) tl
    in
    aux i [] l
