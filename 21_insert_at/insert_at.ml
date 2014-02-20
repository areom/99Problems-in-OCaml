(* nsert an element at a given position into a list. *)
let insert_at it i l =
    (* do it tail recursively *)
    let rec aux i first = function
        | [] -> List.rev (it::first)
        | hd::tl as snd -> if i = 0 then (List.rev first) @ (it :: snd)
                                    else aux (i - 1) (hd::first) tl
    in
    aux i [] l
