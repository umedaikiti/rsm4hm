open Coefficient
open Affine_term
open Matrix
open Pcfg
open Supermartingale

type lp_constraint = (c_coef matrix * c_coef vector) * (p_coef vector * p_coef)

(* val string_of_output_elm : lp_constraint -> string
 * val string_of_output : lp_constraint list -> string *)

(** a compiler based on the theory developed in CAV2018 *)
(* val get_sm_const : pcfg_transition -> config -> invariant -> eta -> l -> lp_constraint option list
 * val get_nonneg_const : invariant -> eta -> l -> lp_constraint
 * val get_scaling_const : config -> eta -> l -> lp_constraint option *)

val lp_seeds_of_reachability_problem
  : reachability_problem -> bool -> lp_constraint list * string list * string
