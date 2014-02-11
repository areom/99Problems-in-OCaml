(* Eliminate consecutive duplicates of list elements. *)
let rec compress = function
  | [] -> []
  | [x] -> [x]
  | head :: (neck :: rest as tail) -> 
      if head = neck then compress tail
                else head :: compress tail
