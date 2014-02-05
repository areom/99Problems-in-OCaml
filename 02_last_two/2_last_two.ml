let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _::tl -> last_two tl
