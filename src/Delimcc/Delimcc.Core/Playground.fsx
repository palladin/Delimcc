#load "Delimcc.fs"

open Delimcc.Core.Delimcc


let test0 = 
    cc { 
        let! x = cc { return 1 }
        return x + 4 
    } |> run |> expect 5



