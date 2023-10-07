
# Delimcc
A monadic library for multi-prompt delimited control in F#. Delimcc is directly based on the work of Oleg Kiselyov.(https://okmij.org/ftp/continuations/implementations.html)

``` fsharp
// Danvy/Filinski's test
// (display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))

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

```
