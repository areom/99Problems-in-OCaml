(*type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

exception Illegal_variable
*)

let rec eval a val_a b val_b = function
    | Var x -> if x = a then val_a
               else if x = b then val_b
               else raise Illegal_variable
    | Not x -> not(eval a val_a b val_b x)
    | And (expr1, expr2) -> (eval a val_a b val_b expr1) &&
                            (eval a val_a b val_b expr2)
    | Or  (expr1, expr2) -> (eval a val_a b val_b expr1) ||
                            (eval a val_a b val_b expr2)

let table2 a b expr = 
    [(true, true, eval a true b true expr);
     (true, false, eval a true b false expr);
     (false, true, eval a false a true expr);
     (false, false, eval a false b false expr)]
 
