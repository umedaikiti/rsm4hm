open Variable
open Coefficient
open Affine_term
open Polytope
open Pcfg

type template = p_coef term LM.t

val generate_template : l_domain -> v_domain -> string -> template * param_domain

val get_pre_expectation : f -> template -> v list * (c_coef polytope * p_coef term) list
val get_pre_condition : f -> template -> v list * (c_coef polytope * p_coef term) list

