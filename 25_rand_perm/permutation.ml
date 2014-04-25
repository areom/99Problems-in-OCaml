(* Generate a random permutation of the elements of a list. *)
let permutaion l = 
    let extract_rand l = 
        let rec extract l i = function
            | [] -> raise Not_found
            | h::t -> if i = 0 then (h, l @ t) else extract (h::l) (i - 1) t
        in
        extract [] (Random.int (List.length l)) l
    in
    let rec do_perm result = function
        | [] -> result
        | l  -> let picked, rest = extract_rand l in
                do_perm (picked::result) rest
    in
    do_perm [] l
