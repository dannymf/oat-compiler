open Ll
open Datastructures                                     

(* escape analysis ---------------------------------------------------------- *)

(* Instantiates the generic dataflow analysis framework with the
   lattice for escape analysis.
     - the lattice elements are sets of LL uids that must have pointer type
     - the flow functions propagate escaping pointers toward their definitions
*)

let is_ptr_arg (ty, o) : operand option =
  match ty with
  | Ptr t -> Some o
  | _ -> None

let uids_of_ops : operand list -> UidS.t =
    List.fold_left (fun s o -> match o with Id u -> UidS.add u s | _ -> s)
      UidS.empty
  

(* escaping operands of a terminator --------------------------------------------- *)
let term_escapes : terminator -> UidS.t = function 
   | Ret (Ptr _, Some Id x) -> UidS.singleton x
   | Ret (_, _)
   | Br _        
   | Cbr (_,_,_) -> UidS.empty

let insn_escapes (out:UidS.t) (u:uid) (i:insn) : UidS.t =
  let conditional_escape u x : UidS.t =
    if UidS.mem u out then UidS.singleton x else UidS.empty 
  in
  match i with
  | Store (Ptr _, Id p, _) -> UidS.singleton p
  | Bitcast(_,Id x,_)      -> conditional_escape u x
  | Gep(_,Id x,_)          -> conditional_escape u x
  | Call(_,_,args)         -> List.filter_map is_ptr_arg args |> uids_of_ops
  | _ -> UidS.empty
  

(* (In our representation, there is one flow function for instructions
   and another for terminators.                                               *)
let insn_flow (u,i:uid * insn) (out:UidS.t) : UidS.t =
  out |> UidS.union (insn_escapes out u i) 

let terminator_flow (t:terminator) (out:UidS.t) : UidS.t =
  out |> UidS.union (term_escapes t)

module Fact =
  struct
    let forwards = false
    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    (* the lattice ---------------------------------------------------------- *)
    type t = UidS.t
    let combine ds = List.fold_left UidS.union UidS.empty ds
    let equal = UidS.equal

    let compare = UidS.compare
    let to_string = UidS.to_string
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (cfg:Cfg.cfg) : Graph.t =
  let init l = UidS.empty in
  let live_out = UidS.empty in
  let g = Graph.of_cfg init live_out cfg in
  Solver.solve g
