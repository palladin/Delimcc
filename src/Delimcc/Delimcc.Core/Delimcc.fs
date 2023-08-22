namespace Delimcc.Core
open System

module Delimcc =
    
    type CC<'a> = ('a -> unit) -> PTop -> unit
    and PTop = Ref<PStack>
    and PStack = list<PFrame>
    and PFrame = { Mark : Mark; EK : unit -> unit }
    and Prompt<'a> = { Mark : Mark; Box : Ref<CC<'a>> }
    and SubCont<'a, 'b> = { PA : Prompt<'a>; PB : Prompt<'b>; PS : list<PFrame> }
    and Mark = int

    type CCBuilder() = 
        member self.Return(x : 'a) : CC<'a> = fun k _ -> k x
        member self.ReturnFrom (m : CC<'a>) : CC<'a> = m
        member self.Bind(m : CC<'a>, f : 'a -> CC<'b>) : CC<'b> = fun k ptop -> m (fun v -> f v k ptop) ptop

    let cc = new CCBuilder()

    let error : string -> 'a = fun msg -> failwith msg
    let run : CC<'a> -> 'a = fun m -> 
        let ptop = ref []
        let ans = ref <| fun () -> error "run: no prompt was ever set!"
        m (fun x -> ans.Value <- fun () -> x) ptop |> ignore
        ans.Value()

    let expect : 'a -> 'a -> unit = 
        fun  ve vp -> if ve = vp then printf "expected answer %A, computed %A " ve vp else error <| sprintf "expected answer %A, computed %A " ve vp
                 
    let assure : CC<bool> -> CC<unit> = fun m ->
        cc {
            let! v = m 
            return if v then error "assertion failed" else ()        
        }

    let counter = ref 0
    let newPrompt<'a> : CC<Prompt<'a>> = fun k ptop -> counter.Value <- counter.Value + 1; k <| { Mark = counter.Value; Box = Unchecked.defaultof<_>  }

    let get_pstack : CC<PStack> = fun k ptop -> k ptop.Value
    let isPromptSet : Prompt<'a> -> CC<bool> = fun p ->
        let rec loop : PStack -> CC<bool> = fun stack ->
            match stack with
            | [] -> cc { return false }
            | (h :: t) -> if p.Mark = h.Mark then cc { return true } else loop t
        cc {
            let! stack = get_pstack
            return! loop stack
        }
        
            

    