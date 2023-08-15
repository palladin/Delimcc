﻿namespace Delimcc.Core

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
        member self.Bind(m : CC<'a>, f : 'a -> CC<'b>) : CC<'b> = fun k ptop -> m (fun v -> f v k ptop) ptop

    let cc = new CCBuilder()

    let error : string -> 'a = fun msg -> failwith msg
    let run : CC<'a> -> 'a = fun m -> 
        let ptop = ref []
        let ans = ref <| fun () -> error "run: no prompt was ever set!"
        m (fun x -> ans.Value <- fun () -> x) ptop |> ignore
        ans.Value()
    