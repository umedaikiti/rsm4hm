open Variable
open Coefficient
open Affine_term
open Polytope


type l
val l_of_string : string -> l
val string_of_l : l -> string

module LM :
  sig
    type key = l
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

 
type l_domain
val l_domain_of_l_list : l list -> l_domain
val l_list_of_l_domain : l_domain -> l list
val string_of_l_domain : l_domain -> string

type linpredmap = c_coef linpred LM.t
val linpredmap_empty : linpredmap
val linpredmap_singleton : l -> c_coef linpred -> linpredmap
val linpredmap_mem : l -> linpredmap -> bool
val linpredmap_add : l -> c_coef linpred -> linpredmap -> linpredmap 
val linpredmap_fold_add : linpredmap -> linpredmap -> linpredmap 

val string_of_linpredmap : linpredmap -> string
(*
type invariant = c_coef linpred LM.t
val string_of_invariant : invariant -> string
 *)
type config = l * v valuation

type rewardmap = float LM.t
val rewardmap_empty : rewardmap
val rewardmap_singleton : l -> float -> rewardmap
val rewardmap_mem : l -> rewardmap -> bool
val rewardmap_add : l -> float -> rewardmap -> rewardmap 
val rewardmap_fold_add : rewardmap -> rewardmap -> rewardmap 
val string_of_rewardmap : rewardmap -> string

type distr =
  | Unif of float * float
  | Geom of float
  | Norm of float * float
  | Disc of (float * float) list (* f1|p1> + ... + fn|pn> *)
val string_of_distr : distr -> string

val expectation : distr -> float
val get_range : distr -> v -> c_coef linpred

type assgn =
  | Prob_assgn of v * distr
  | Nondet_assgn of  c_coef linpred
  | Det_assgn of v * c_coef term
val string_of_assgn : assgn -> string
val string_of_assgn' : assgn -> string


type 'a fDist = ('a * float) list
val string_of_fDist : ('a -> string) -> 'a fDist -> string
val get_prob : 'a fDist -> 'a -> float
val fDist_of_list : ('a * float) list -> 'a fDist

(*
type guard = c_coef polytope list
val guard_of_polytope_list : c_coef_polytope list -> guard
val string_of_guard : guard -> string
val guard_true: guard
 *)

type f =
  | A of assgn * l    (** lpre |-i,x:=1-> l *)
  | P of l fDist           (** lpre |--> l1 w/ p, l2 w/ 1-p *)
  | ND of (c_coef linpred * l) list (** lpre |-x>=0-> l1, |-x<0-> l2 *)
val string_of_f : f -> string

type pcfg_transition = f LM.t
type pcfg = pcfg_transition * l_domain * v_domain
type reachability_problem = pcfg * int * config * linpredmap * linpredmap * rewardmap

val pcfg_transition_empty : pcfg_transition
val pcfg_transition_singleton : l -> f -> pcfg_transition
val pcfg_transition_add : l -> f -> pcfg_transition -> pcfg_transition
val pcfg_transition_fold_add : pcfg_transition -> pcfg_transition -> pcfg_transition
