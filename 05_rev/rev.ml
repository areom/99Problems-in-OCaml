(* Reverse a list. *)
let rev l =
  let rec aux result = function
    | [] -> result
    | h::t -> aux (h::result) t in
  aux [] l
