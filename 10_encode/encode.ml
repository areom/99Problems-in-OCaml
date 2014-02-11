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

let encode_alt l =
    let pack l = 
        let rec aux current result = function
            | []  -> []
            | [x] -> (x :: current) :: result
            | hd :: (nk :: _ as tl) -> if hd = nk 
                                       then aux (hd :: current) result tl
                                       else aux [] ((hd :: current) :: result) tl
        in
        List.rev (aux [] [] l)
    in    
    List.map (fun l -> (List.length l, List.hd l)) (pack l)
