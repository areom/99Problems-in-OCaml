type 'a rle =
    | One  of 'a
    | Many of int * 'a

let encode_mod l = 
    let rec aux count result = function
        | [] -> []
        | [x] -> Many (count, x) :: result
        | head :: (neck :: _ as tail) -> 
            if head = neck 
            then aux (count + 1) result tail
            else if count = 1 
                 then aux 1 ((One head)::result) tail
                 else aux 1 (Many (count, head)::result) tail
    in
    List.rev (aux 1 [] l)
