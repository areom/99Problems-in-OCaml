(* Group the elements of a set into disjoint subsets. *)
(*
let rec combination n = function
    | []     -> []
    | [x]    -> if n = 1 then [[x]] else []
    | hd::tl -> let listify item = [item] in
                let join item ls = item::ls in
                if n = 1 then List.map listify (hd::tl)
                         else (List.map (join hd) (combination (n-1) tl)) @ (combination n tl)
*)

let group list sizes =
    let initial = List.map (fun size -> size, []) sizes in
    let prepend p list =
        let emit l acc = l :: acc in
        let rec aux emit acc = function
            | [] -> emit [] acc
            | (n,l) as h::t -> let acc = if n > 0 then emit ((n-1, p::l) :: t) acc
                                        else acc in
                               aux (fun l acc -> emit (h::l) acc) acc t
        in
        aux emit [] list
    in
    let rec aux = function
        | [] -> [initial]
        | h::t -> List.concat (List.map (prepend h) (aux t))
    in
    let all = aux list in
    let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
    List.map (List.map snd) complete
