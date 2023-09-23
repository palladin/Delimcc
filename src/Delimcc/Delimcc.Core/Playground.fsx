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

let test2 () =
    cc {
        let! p = newPrompt
        let! x = pushPrompt p (pushPrompt p <| cc { return 5 })
        return 4 + x
    } |> run |> expect 9

let test3 () = 
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc { 
            let! x = abortP p <| cc { return 5 }
            return 6 + x
        }
        return 4 + x
    } |> run |> expect 9

let test3' () = 
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc { 
            return! pushPrompt p <| cc { 
                let! x = abortP p <| cc { return 5 }
                return 6 + x
            }
        }
        return 4 + x
    } |> run |> expect 9

let test3'1 () = 
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc { 
            return! pushPrompt p <| cc { 
                let! x = takeSubCont p (fun _ -> cc { return 5 })
                return 6 + x
            }
        }
        return 4 + x
    } |> run |> expect 9

[test0; test1; test2; test3; test3'; test3'1] |> List.iter (fun f -> f ())
