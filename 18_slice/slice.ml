(* Extract a slice from a list with both limits included. *)
let slice l i k =
    let rec sublist current result = function
        | [] -> List.rev result
        | hd::tl -> if current < i then sublist (current+1) result tl else
                        if current > k then List.rev result else sublist (current+1) (hd::result) tl
    in
    sublist 0 [] l

(* Following is the 'official' solution *)
let rec fold_until f acc n = function
    | [] -> (acc, [])
    | hd :: tl -> if n = 1 then ((f acc hd), tl)
                       else fold_until f (f acc hd) (n-1) tl

let slice2 l i k =
    let _, l = fold_until (fun _ _ -> []) [] i l in
    let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) l in
    List.rev taken
