(* Duplicate the elements of a list. *)
(* tail recursive *)
let duplicate l = 
    let rec aux result = function
        | [] -> result
        | hd::tl -> aux (hd::(hd::result)) tl
    in
    List.rev (aux [] l)
