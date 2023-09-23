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

    let pure' : 'a -> CC<'a> = fun x k _ -> k x
    let (>>=) : CC<'a> -> ('a -> CC<'b>) -> CC<'b> = fun m f k ptop ->
        m (fun v -> f v k ptop) ptop

    type CCBuilder() = 
        member self.Return(x : 'a) : CC<'a> = pure' x
        member self.ReturnFrom (m : CC<'a>) : CC<'a> = m
        member self.Bind(m : CC<'a>, f : 'a -> CC<'b>) : CC<'b> = m >>= f

    let cc = new CCBuilder()

    let error : string -> 'a = fun msg -> failwith msg
    let run : CC<'a> -> 'a = fun m -> 
        let ptop = ref []
        let ans = ref <| fun () -> error "run: no prompt was ever set!"
        m (fun x -> ans.Value <- fun () -> x) ptop |> ignore
        ans.Value()

    let expect : 'a -> 'a -> unit = 
        fun  ve vp -> if ve = vp then printfn "expected answer %A, computed %A " ve vp else error <| sprintf "expected answer %A, computed %A " ve vp
                 
    let assure : CC<bool> -> CC<unit> = fun m ->
        cc {
            let! v = m 
            return if v then () else error "assertion failed"
        }

    let counter = ref 0
    let newPrompt<'a> : CC<Prompt<'a>> = fun k ptop -> counter.Value <- counter.Value + 1; k <| { Mark = counter.Value; Box = ref Unchecked.defaultof<_>  }

    let getPStack : CC<PStack> = fun k ptop -> k ptop.Value

    let getPFrame : PTop -> PFrame = fun ptop ->
        match ptop.Value with
        | [] -> error "Empty PStack! Can't be happening"
        | h :: _ -> h

    let pushPFrame : PTop -> PFrame -> unit = fun ptop pframe -> 
        let stack = ptop.Value
        ptop.Value <- pframe :: stack

    let popPFrame : PTop -> PFrame = fun ptop ->
        match ptop.Value with
        | [] -> error "Empty PStack! Can't be happening"
        | h :: t -> 
            ptop.Value <- t
            h

    let popPrompt : Prompt<'a> -> CC<'a> = fun p k ptop ->
        //printfn "ptop: %A" ptop.Value
        let _ = popPFrame ptop
        let cc = p.Box.Value
        cc k ptop

    let pushPrompt : Prompt<'a> -> CC<'a> -> CC<'a> = fun p body k ptop ->
        let ek = fun () -> popPrompt p k ptop
        pushPFrame ptop { Mark = p.Mark; EK = ek} 
        body (fun res -> 
                p.Box.Value <- pure' res
                let pframe = getPFrame ptop
                pframe.EK ()) ptop

    let unwindAbort : Mark -> PStack -> (PFrame * PStack) = fun mark stack -> 
        let rec loop : PStack -> (PFrame * PStack) = fun stack ->
            match stack with
            | [] -> error "No prompt was set" 
            | (h :: t) as s -> if mark = h.Mark then (h, s) else loop t
        loop stack

    let abortP : Prompt<'a> -> CC<'a> -> CC<'b> = fun p res k ptop -> 
        let stack = ptop.Value
        let (h, s) = unwindAbort p.Mark stack
        ptop.Value <- s
        p.Box.Value <- res
        h.EK ()

    let isPromptSet : Prompt<'a> -> CC<bool> = fun p ->
        let rec loop : PStack -> CC<bool> = fun stack ->
            match stack with
            | [] -> pure' false 
            | (h :: t) -> if p.Mark = h.Mark then pure' true else loop t
        cc {
            let! stack = getPStack
            return! loop stack
        }
        
            

    