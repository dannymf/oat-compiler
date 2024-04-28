open Ll
open Datastructures

(* The abstract domain of symbolic constants -------------------------------- *)
module SymConst = struct
  type t =
    | NonConst (* Uid may take on multiple values at runtime *)
    | Const of int64 (* Uid will always evaluate to const i64 or i1 *)
    | UndefConst (* Uid is not defined at the point *)

  let compare (a : t) (b : t) =
    match a, b with
    | Const i, Const j -> Int64.compare i j
    | NonConst, NonConst | UndefConst, UndefConst -> 0
    | NonConst, _ | _, UndefConst -> 1
    | UndefConst, _ | _, NonConst -> -1
  ;;

  let to_string : t -> string = function
    | NonConst -> "NonConst"
    | Const i -> Printf.sprintf "Const (%LdL)" i
    | UndefConst -> "UndefConst"
  ;;
end

(* The analysis computes, at each program point, which UIDs in scope will evaluate
   to integer constants *)
type fact = SymConst.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
*)
let eval_bop bop a b =
  match bop with
  | Add -> Int64.add a b
  | Sub -> Int64.sub a b
  | Mul -> Int64.mul a b
  | Shl -> Int64.shift_left a (Int64.to_int b)
  | Lshr -> Int64.shift_right_logical a (Int64.to_int b)
  | Ashr -> Int64.shift_right a (Int64.to_int b)
  | And -> Int64.logand a b
  | Or -> Int64.logor a b
  | Xor -> Int64.logxor a b
;;

let eval_cnd cnd a b =
  let wrap boo = if boo then 1L else 0L in
  match cnd with
  | Eq -> wrap @@ Int64.equal a b
  | Ne -> wrap @@ not @@ Int64.equal a b
  | Slt -> wrap (Int64.compare a b < 0)
  | Sle -> wrap (Int64.compare a b <= 0)
  | Sgt -> wrap (Int64.compare a b > 0)
  | Sge -> wrap (Int64.compare a b >= 0)
;;

let insn_flow ((u, i) : uid * insn) (d : fact) : fact =
  match i with
  | Binop (bop, _, op1, op2) ->
    (match op1, op2 with
     | Const a, Const b -> UidM.add u (SymConst.Const (eval_bop bop a b)) d
     | Const a, Id id2 ->
       (try
          match UidM.find id2 d with
          | SymConst.Const b -> UidM.add u (SymConst.Const (eval_bop bop a b)) d
          | SymConst.UndefConst -> UidM.add u SymConst.UndefConst d
          | SymConst.NonConst -> UidM.add u SymConst.NonConst d
        with
        | Not_found -> UidM.add u SymConst.UndefConst d)
     | Id id1, Const b ->
       (try
          match UidM.find id1 d with
          | SymConst.Const a -> UidM.add u (SymConst.Const (eval_bop bop a b)) d
          | SymConst.UndefConst -> UidM.add u SymConst.UndefConst d
          | SymConst.NonConst -> UidM.add u SymConst.NonConst d
        with
        | Not_found -> UidM.add u SymConst.UndefConst d)
     | Id id1, Id id2 ->
       (try
          match UidM.find id1 d, UidM.find id2 d with
          | SymConst.Const a, SymConst.Const b ->
            UidM.add u (SymConst.Const (eval_bop bop a b)) d
          | SymConst.UndefConst, _ | _, SymConst.UndefConst ->
            UidM.add u SymConst.UndefConst d
          | SymConst.NonConst, _ | _, SymConst.NonConst -> UidM.add u SymConst.NonConst d
        with
        | Not_found -> UidM.add u SymConst.UndefConst d)
     | _ -> failwith "non id/const operands")
  | Icmp (cnd, _, op1, op2) ->
    (match op1, op2 with
     | Const a, Const b -> UidM.add u (SymConst.Const (eval_cnd cnd a b)) d
     | Const a, Id id2 ->
       (try
          match UidM.find id2 d with
          | SymConst.Const b -> UidM.add u (SymConst.Const (eval_cnd cnd a b)) d
          | SymConst.UndefConst -> UidM.add u SymConst.UndefConst d
          | SymConst.NonConst -> UidM.add u SymConst.NonConst d
        with
        | Not_found -> UidM.add u SymConst.UndefConst d)
     | Id id1, Const b ->
       (try
          match UidM.find id1 d with
          | SymConst.Const a -> UidM.add u (SymConst.Const (eval_cnd cnd a b)) d
          | SymConst.UndefConst -> UidM.add u SymConst.UndefConst d
          | SymConst.NonConst -> UidM.add u SymConst.NonConst d
        with
        | Not_found -> UidM.add u SymConst.UndefConst d)
     | Id id1, Id id2 ->
       (try
          match UidM.find id1 d, UidM.find id2 d with
          | SymConst.Const a, SymConst.Const b ->
            UidM.add u (SymConst.Const (eval_cnd cnd a b)) d
          | SymConst.UndefConst, _ | _, SymConst.UndefConst ->
            UidM.add u SymConst.UndefConst d
          | SymConst.NonConst, _ | _, SymConst.NonConst -> UidM.add u SymConst.NonConst d
        with
        | Not_found -> UidM.add u SymConst.UndefConst d)
     | _ -> failwith "non id/const operands")
  | Store _ | Call (Void, _, _) -> UidM.add u SymConst.UndefConst d
  | _ -> UidM.add u SymConst.NonConst d
;;

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t : terminator) (d : fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact = struct
  type t = fact

  let forwards = true
  let insn_flow = insn_flow
  let terminator_flow = terminator_flow
  let normalize : fact -> fact = UidM.filter (fun _ v -> v != SymConst.UndefConst)

  let compare (d : fact) (e : fact) : int =
    UidM.compare SymConst.compare (normalize d) (normalize e)
  ;;

  let to_string : fact -> string = UidM.to_string (fun _ v -> SymConst.to_string v)

  (* The constprop analysis should take the join over predecessors to compute the
     flow into a node. You may find the UidM.merge function useful *)
  let combine (ds : fact list) : fact =
    let f fact1 fact2 =
      match fact1, fact2 with
      | SymConst.Const a, SymConst.Const b when Int64.compare a b = 0 -> SymConst.Const a
      | SymConst.Const _, SymConst.Const _ -> SymConst.NonConst
      | SymConst.NonConst, _ | _, SymConst.NonConst -> SymConst.NonConst
      | SymConst.UndefConst, _ | _, SymConst.UndefConst -> SymConst.UndefConst
    in
    List.fold_left
      (fun map new_fact ->
        UidM.merge
          (fun _ v1 v2 ->
            match v1, v2 with
            | Some fact1, Some fact2 -> Some (f fact1 fact2)
            | Some fact1, None | None, Some fact1 -> Some fact1
            | None, None -> None)
          map
          new_fact)
      UidM.empty
      ds
  ;;
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g : Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in
  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in =
    List.fold_right (fun (u, _) -> UidM.add u SymConst.NonConst) g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg
;;

(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper
   functions. *)
let run (cg : Graph.t) (cfg : Cfg.t) : Cfg.t =
  let open SymConst in
  let cp_block (l : Ll.lbl) (cfg : Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    failwith "Constprop.cp_block unimplemented"
  in
  LblS.fold cp_block (Cfg.nodes cfg) cfg
;;
