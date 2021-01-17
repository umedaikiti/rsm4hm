open Variable
open Coefficient
open Matrix
open Pcfg
open Lp_constraint

type progmath = string

(* val progmath_of_c_coef : c_coef -> progmath
 * val progmath_of_c_coef_vec_string_list_mult : c_coef vector -> string list -> progmath
 * val progmath_of_p_coef : p_coef -> progmath
 *   
 * val such_that_y_list_of_output_elm : lp_constraint -> string list * string list
 * 
 * val progmath_of_mat_vecs : lp_constraint list ->  string * string list *)

val progmath_of_reachability_problem : reachability_problem -> bool -> progmath
