let slice l i k =
    let rec sublist current result = function
        | [] -> List.rev result
        | hd::tl -> if current < i then sublist (current+1) result tl else
                        if current > k then List.rev result else sublist (current+1) (hd::result) tl
    in
    sublist 0 [] l
