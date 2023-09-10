#load "Delimcc.fs"

open Delimcc.Core.Delimcc


let test0 () = 
    cc { 
        let! x = cc { return 1 }
        return x + 4 
    } |> run |> expect 5



let test1 () = 
    cc {
        let! p = newPrompt
        do! assure <| cc { 
            let! b = isPromptSet p             
            return not b
        }
        return! pushPrompt p <| cc {            
            do! assure <| isPromptSet p               
            return 1            
        }
    } |> run |> expect 1 



[test0; test1] |> List.iter (fun f -> f ())
