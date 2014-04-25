(* Generate the combinations of K distinct objects chosen from the N elements of a list. *)
let rec combination n = function
    | []     -> []
    | [x]    -> if n = 1 then [[x]] else []
    | hd::tl -> let listify item = [item] in
                let join item ls = item::ls in
                if n = 1 then List.map listify (hd::tl)
                         else (List.map (join hd) (combination (n-1) tl)) @ (combination n tl)


let extract k list =
    let rec aux k acc emit = function
      | [] -> acc
      | h :: t ->
        if k = 1 then aux k (emit [h] acc) emit t else
          let new_emit x = emit (h :: x) in
          aux k (aux (k-1) acc new_emit t) emit t
    in
    let emit x acc = x :: acc in
    aux k [] emit list
