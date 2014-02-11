(* Run-length encoding of a list (direct solution). *)
(* I don't think this is any different from #11. *)
type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode_direct l =
    let tuple count x = if count = 1 then One x else Many (count, x) in
    let rec aux count result = function
        | [] -> []
        | [x] -> (tuple count x) :: result
        | head :: (neck :: _ as tail) -> if head = neck then aux (count+1) result tail
                                                        else aux 1 ((tuple count head)::result) tail
    in
    List.rev (aux 1 [] l)
