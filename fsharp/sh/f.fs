let print a = 
    printfn "%d" a

let rec cycle b e = 
    if (b > e) then (print b)
    else  cycle (b + 1) e

[<EntryPoint>]
let main argv = 
    cycle 1 10
    0 // return
