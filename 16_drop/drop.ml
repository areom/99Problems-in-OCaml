let drop l n =
    let rec aux current result = function
        | [] -> result
        | hd::tl -> if current = 1 then aux n result tl
                                   else aux (current - 1) (hd :: result) tl
    in
    List.rev (aux n [] l)
