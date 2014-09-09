open Unix

let timeit f a = 
    let t0 = Unix.gettimeofday() in
    ignore(f a);
    let t1 = Unix.gettimeofday() in
    t1 -. t0
