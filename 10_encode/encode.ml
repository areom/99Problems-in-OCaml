(* Run-length encoding of a list. *)
let encode l = 
    let rec aux count result = function
        | [] -> []
        | [x] -> (count, x) :: result
        | head :: (neck :: _ as tail) -> if head = neck
                                        then aux (count+1) result tail
                                        else aux 1 ((count, head) :: result) tail
    in
    List.rev (aux 1 [] l)
