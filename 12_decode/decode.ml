(* Decode a run-length encoded list. *)
type 'a rle =
    | One  of 'a
    | Many of int * 'a

let decode l = 
    let rec expand n x result =
        if n = 0 then result else expand (n-1) x (x :: result)
    in
    let rec aux result = function
        | [] -> result
        | One head :: tail -> aux (head::result) tail
        | Many (n, x) :: tail -> aux (expand n x result) tail
    in
    List.rev (aux [] l)
