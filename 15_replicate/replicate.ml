(* Replicate the elements of a list a given number of times. *)
let replicate l times =
    let rec add n x result =
        if n = 0 then result else add (n-1) x (x :: result)
    in
    let rec do_replicate n result = function
        | [] -> result
        | hd::tl -> do_replicate n (add n hd result) tl
    in
    List.rev (do_replicate times [] l)
