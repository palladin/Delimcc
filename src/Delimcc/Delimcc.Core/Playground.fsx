#load "Delimcc.fs"

open Delimcc.Core.Delimcc

let test = cc { return 42 } 

run test

