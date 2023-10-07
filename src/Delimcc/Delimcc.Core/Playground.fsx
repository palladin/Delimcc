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

let test3'' () = 
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc {
            let! v1 = pushPrompt p <| cc { 
                let! x = abortP p <| cc { return 5 }
                return x + 6
            }
            let! v2 = abortP p <| cc { return 7 }
            return v1 + v2 + 10
        }
        return x + 20
    } |> run |> expect 27

let test3''1 () = 
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc {
            let! v1 = pushPrompt p <| cc { 
                let! x = takeSubCont p (fun _ -> cc { return 5 })
                return x + 6
            }
            let! v2 = takeSubCont p (fun _ -> cc { return 7 })
            return v1 + v2 + 10
        }
        return x + 20
    } |> run |> expect 27

let test4 () =
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc {            
            let! x = takeSubCont p <| fun sk -> cc { 
                return! pushPrompt p <| cc { 
                    return! pushSubCont sk <| cc { return 5 }                    
                }
            }
            return x + 10
        }
        return x + 20
    } |> run |> expect 35

let test41 () =
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc {            
            let! x = takeSubCont p <| fun sk -> cc { 
                return! pushSubCont sk <| cc { 
                    return! pushPrompt p <| cc { 
                        return! pushSubCont sk <| cc {
                            return! abortP p <| cc { return 5 }
                        }
                    }
                }
            }
            return x + 10
        }
        return x + 20
    } |> run |> expect 35

let test5 () = 
    cc {
        let! p = newPrompt
        let! x = pushPrompt p <| cc {
            let! x = shiftP p <| fun sk -> cc {
                let! x = sk 3
                let! x' = sk x
                return x' + 100
            }
            return x + 2
        }
        return x + 10
    } |> run |> expect 117

let testls () = 
    cc {
        let! p = newPrompt
        return! pushPrompt p <| cc {
            let! xv = shiftP p <| fun f -> cc { 
                let! xv = f [] 
                return "a" :: xv
            }
            return! shiftP p <| fun _ -> cc { return xv }
        }
    } |> run |> expect ["a"]


let testlc () = 
    cc {
        let! p = newPrompt
        return! pushPrompt p <| cc {
            let! xv = controlP p <| fun f -> cc { 
                let! xv = f [] 
                return "a" :: xv
            }
            return! controlP p <| fun _ -> cc { return xv }
        }
    } |> run |> expect []

let testlc' () = 
    cc {
        let! p = newPrompt
        return! pushPrompt p <| cc {
            let! xv = controlP p <| fun f -> cc { 
                let! xv = f [] 
                return "a" :: xv
            }
            return! controlP p <| fun g -> cc { return! g xv }
        }
    } |> run |> expect ["a"]

[test0; test1; test2; test3; test3'; test3'1; test3''; test3''1; 
 test4; test41; test5; testls; testlc; testlc'] |> List.iter (fun f -> f ())
