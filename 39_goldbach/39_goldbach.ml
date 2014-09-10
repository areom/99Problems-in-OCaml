(* A list of Goldbach compositions.

Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000. *)

let is_prime n =
    let n = abs n in
    let rec divisible k =
        match n mod k with
        | 0 when n <> 2 -> false
        | _ -> if k * k > n then true
               else divisible (k+1)
    in
    divisible 2

let goldbach n =
    let rec tryout d =
        if is_prime d && is_prime (n - d) then (d, n-d) else tryout (d+1)    
    in
    tryout 2

let rec goldbach_list low high = 
    let low = (low + 1) / 2 * 2 in
    let high = (high + 1) / 2 * 2 in
    if low > high then []
        else (low, goldbach low) :: goldbach_list (low+2) high

let goldbach_limit low high t =
    List.filter (fun (_, (a, b)) -> a > t && b > t) (goldbach_list low high)
