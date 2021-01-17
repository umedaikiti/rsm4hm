open Util
open Variable
open Coefficient
open Affine_term
open Polytope
open Matrix


type l = string
let l_of_string x = x
let string_of_l l = l

module LM=
  Map.Make
    (struct
      type t = l
      let compare = compare
    end)


type l_domain = string list
let normalize_l_domain = remove_duplicates
let l_domain_of_l_list list = normalize_l_domain list
let l_list_of_l_domain x = x
let string_of_l_domain l_domain = string_of_list string_of_l l_domain

type linpredmap = c_coef linpred LM.t
let linpredmap_empty : linpredmap = LM.empty
let linpredmap_singleton (l : l) (linpred : c_coef linpred) : linpredmap = LM.singleton l linpred
let linpredmap_mem (l : l) (linpredmap : linpredmap) : bool = LM.mem l linpredmap
let linpredmap_add (l : l) (linpred : c_coef linpred) (linpredmap : linpredmap) : linpredmap = LM.add l linpred linpredmap 
let linpredmap_fold_add (linpredmap1 : linpredmap) (linpredmap2 : linpredmap) : linpredmap =
  LM.fold (fun l f linpredmap2 -> LM.add l f linpredmap2) linpredmap1 linpredmap2


let string_of_linpredmap (linpredmap: linpredmap) : string = "(linpredmap: " ^ string_of_list (string_of_pair string_of_l (string_of_c_coef_linpred)) (LM.bindings linpredmap) ^ ")"
(* type invariant = c_coef linpred LM.t
let string_of_invariant (invariant : invariant) : string = "(invariant: " ^ string_of_list (string_of_pair string_of_l (string_of_c_coef_linpred)) (LM.bindings invariant) ^ ")"
 *)
type config = l * v valuation

type rewardmap = float LM.t
let rewardmap_empty : rewardmap = LM.empty
let rewardmap_singleton (l : l) (reward : float) : rewardmap = LM.singleton l reward
let rewardmap_mem (l : l) (rewardmap : rewardmap) : bool = LM.mem l rewardmap
let rewardmap_add (l : l) (reward : float) (rewardmap : rewardmap) : rewardmap = LM.add l reward rewardmap 
let rewardmap_fold_add (rewardmap1 : rewardmap) (rewardmap2 : rewardmap) : rewardmap =
  LM.fold (fun l f rewardmap2 -> LM.add l f rewardmap2) rewardmap1 rewardmap2
let string_of_rewardmap (rewardmap: rewardmap) : string = "(rewardmap: " ^ string_of_list (string_of_pair string_of_l string_of_float) (LM.bindings rewardmap) ^ ")"

type distr =
  | Unif of float * float
  | Geom of float
  | Norm of float * float
  | Disc of (float * float) list
let string_of_distr (distr: distr) : string = match distr with
  | Unif (p1, p2) -> "Unif" ^ string_of_float p1 ^ string_of_float p2
  | Geom p -> "Geom" ^ string_of_float p
  | Norm (p1, p2) -> "Norm" ^ string_of_float p1 ^ string_of_float p2
  | Disc list -> "Disc" ^ string_of_list (string_of_pair string_of_float string_of_float) list

let expectation (distr: distr) : float = match distr with
| Unif (param1, param2) -> (param1 +. param2) /. 2.
| Geom param -> 1. /. param
| Norm (param1, param2) -> param1
| Disc discrete_distr ->
   List.fold_left
     (fun expectation (x,p)  -> expectation +. x *. p)
     0.
     discrete_distr

let get_range (distr: distr) (v: v): c_coef linpred = match distr with
| Unif (param1, param2) -> linpred_of_polytope_list [polytope_of_ineq_list [c_coef_ineq_of_c_coef_terms (c_coef_term_of_float param1, LEQ, c_coef_term_of_v v); c_coef_ineq_of_c_coef_terms (c_coef_term_of_v v , LEQ, c_coef_term_of_float param2)]]
| Geom param -> linpred_of_polytope_list [polytope_of_ineq_list [c_coef_ineq_of_c_coef_terms (c_coef_term_of_v v, LEQ, c_coef_term_of_float param)]]
| Norm (param1, param2) -> linpred_of_polytope_list [true_c_coef_polytope]
| Disc discrete_distr ->
   linpred_of_polytope_list
     (List.map
        (fun (x, _) ->
          polytope_of_ineq_list
            [c_coef_ineq_of_c_coef_terms (c_coef_term_of_float x, LEQ, c_coef_term_of_v v);
             c_coef_ineq_of_c_coef_terms (c_coef_term_of_float x, GEQ, c_coef_term_of_v v)])
        discrete_distr)


(*
type interval =
  DInt | DReal | DIntInterval of int * int | DRealInterval of const * const | DOr of dom * dom 
type pcfg_dom = interval list
 *)
type assgn = 
  | Prob_assgn of v * distr
  | Nondet_assgn of  c_coef linpred
  | Det_assgn of v * c_coef term
let string_of_assgn (assgn: assgn) : string = match assgn with
  | Prob_assgn (v,distr) -> "Prob_asgn (" ^ string_of_v v ^ ", " ^ string_of_distr distr ^ ")"
  | Nondet_assgn pcfg_dom -> "Nondet_asgn (" ^ string_of_c_coef_linpred pcfg_dom ^ ")"
  | Det_assgn (v,term) -> "Det_asgn (" ^ string_of_v v ^ ", " ^ string_of_c_coef_term term ^ ")"
let string_of_assgn' (assgn: assgn) : string = match assgn with
  | Prob_assgn (v,distr) -> string_of_v v ^ " ~ " ^ string_of_distr distr
  | Nondet_assgn pcfg_dom -> string_of_c_coef_linpred' pcfg_dom
  | Det_assgn (v,term) -> string_of_v v ^ " := " ^ string_of_c_coef_term' term

(*
let sup (nondet_expr: c_coef polytope) : float =
(** TODO: study polytope for the whole implementation *)
  let matrix, vector = split_c_coef_polytope nondet_expr in
  if List.length vector = 1 then
    let candidate = uminus_c_coef (List.hd vector) in
    let divisor_list = List.hd matrix in
    let c_coef =
      List.fold_left 
        max
        (div_c_coef candidate (List.hd divisor_list))
        (List.map (fun x -> div_c_coef candidate x) (List.tl divisor_list)) in
    float_of_c_coef c_coef
  else
    raise (Unsupported "only 1 variable case is supported currently")
 *)

type 'a fDist = ('a * float) list
let string_of_fDist (string_of_a: 'a -> string) (fDist: 'a fDist) : string = string_of_list (string_of_pair string_of_a string_of_float) fDist
let get_prob (fdist: 'a fDist) (a: 'a) : float = List.assoc a fdist
let fDist_of_list l = l

(* type guard = c_coef polytope list 
let string_of_guard (guard: guard) : string = "(guard: " ^ String.concat "," (List.map string_of_c_coef_polytope guard) ^ ")"
let guard_of_c_coef_polytope_list polytope_list = 
let guard_true = [true_c_coef_polytope]
*)

type f =
  | A of assgn * l    (** lpre |-i,x:=1-> l *)
  | P of l fDist           (** lpre |--> l1 w/ p, l2 w/ 1-p *)
  | ND of (c_coef linpred * l) list (** lpre |-x>=0-> l1, |-x<0-> l2 *)
let string_of_f (f: f) : string = match f with
  | A (a,l) -> "A (" ^ string_of_assgn a ^ ", " ^ string_of_l l ^ ")"
  | P f -> "P " ^ string_of_fDist string_of_l f
  | ND f -> "ND " ^ string_of_list (string_of_pair string_of_c_coef_linpred string_of_l) f
type pcfg_transition = f LM.t
type pcfg = pcfg_transition * l_domain * v_domain
type reachability_problem = pcfg * int * config * linpredmap * linpredmap * rewardmap

let pcfg_transition_empty : pcfg_transition = LM.empty
let pcfg_transition_singleton (l : l) (f : f) : pcfg_transition = LM.singleton l f
let pcfg_transition_add (l : l) (f : f) (pcfg_transition : pcfg_transition) : pcfg_transition = LM.add l f pcfg_transition
let pcfg_transition_fold_add (pcfg_transition1 : pcfg_transition) (pcfg_transition2 : pcfg_transition) : pcfg_transition =
  LM.fold (fun l f pcfg_transition2 -> LM.add l f pcfg_transition2) pcfg_transition1 pcfg_transition2
(* 
  let merge_l_list l_list1 l_list2 = function
    | Some (A (v1,assgn1,l1)), Some (A (v,assgn,l)) -> 
Some (List.rev_append l_list1 l_list2)
    | None, Some f | Some f, None -> Some f
    | None, None -> None in
  Pcfg.LM.merge merge_l_list pcfg_transition1 pcfg_transition2
 *)
