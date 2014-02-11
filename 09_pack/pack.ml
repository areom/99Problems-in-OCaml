(* Pack consecutive duplicates of list elements into sublist. *)
let pack l = 
  let rec aux current result = function
    | []        -> current :: result
    | [x]       -> if (List.hd current) = x 
                   then aux (x::current) result []
                   else aux [x] (current :: result) []
    | hd :: tl  -> if (List.hd current) = hd
                   then aux (hd :: current) result tl
                   else aux [hd] (current :: result) tl
  in
  List.rev (aux [(List.hd l)] [] (List.tl l))

let pack2 l = 
  let rec aux current result = function
    | []  -> []
    | [x] -> (x :: current) :: result
    | hd :: (nk :: _ as tl) -> if hd = nk 
                               then aux (hd :: current) result tl
                               else aux [] ((hd :: current) :: result) tl
  in
  List.rev (aux [] [] l)
