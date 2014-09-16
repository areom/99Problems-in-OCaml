(* Find the number of elements of a list. *)
let rec length = function
  | [] -> 0
  | _::t -> (length t)+1

let length_tr list = 
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t in
  aux 0 list
