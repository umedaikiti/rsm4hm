open Variable
open Coefficient
open Affine_term

type 'coef scalar
val coef_of_scalar : 'coef scalar -> 'coef
(* val is_c_coef_scalar : c_coef * v -> bool
 * val is_p_coef_scalar : p_coef * v -> bool *)

(** === inequality, polytope over coefficient as expressions === *)
(* inequality, polytope cannot be minused due to its semantics *)
(* === form terms from inequality === *)
type bop = LEQ | GEQ
val string_of_bop: bop -> string

(** ============ *)
(** === ineq === *)
(** ============ *)
type 'coef ineq

val substitute_v_with_v_in_ineq : 'coef ineq -> v -> v -> 'coef ineq

val string_of_c_coef_ineq : c_coef ineq -> string
val string_of_c_coef_ineq' : c_coef ineq -> string
val true_c_coef_ineq : c_coef ineq
val false_c_coef_ineq : c_coef ineq
val uminus_c_coef_ineq : c_coef ineq -> c_coef ineq
val c_coef_ineq_of_c_coef_terms : c_coef term * bop * c_coef term -> c_coef ineq
val c_coef_term_of_c_coef_ineq : c_coef ineq -> c_coef term
(* \sum a_i x_i <= b_i *)
val c_coef_scalar_of_c_coef_ineq : c_coef ineq -> c_coef scalar
val wo_c_coef_scalar_of_c_coef_ineq : v list -> c_coef ineq -> c_coef list

val string_of_p_coef_ineq : p_coef ineq -> string
val string_of_p_coef_ineq' : p_coef ineq -> string
val true_p_coef_ineq : p_coef ineq
val false_p_coef_ineq : p_coef ineq
val uminus_p_coef_ineq : p_coef ineq -> p_coef ineq
val p_coef_ineq_of_p_coef_terms : p_coef term * bop * p_coef term -> p_coef ineq
val p_coef_term_of_p_coef_ineq : p_coef ineq -> p_coef term
(* \sum a_i x_i <= b_i *)
val p_coef_scalar_of_p_coef_ineq : p_coef ineq -> p_coef scalar
val wo_p_coef_scalar_of_p_coef_ineq : v list -> p_coef ineq -> p_coef list

(** ================ *)
(** === polytope === *)
(** ================ *)
type 'coef polytope

val substitute_v_with_v_in_polytope : 'coef polytope -> v -> v -> 'coef polytope
val v_list_of_polytope : 'coef polytope -> v list

val unit_polytope : 'coef polytope
val add_ineq_to_polytope : 'coef ineq -> 'coef polytope -> 'coef polytope
val polytope_of_ineq_list : 'coef ineq list -> 'coef polytope
val polytope_of_polytope_list : 'coef polytope list -> 'coef polytope
val ineq_list_of_polytope : 'coef polytope -> 'coef ineq list

val string_of_c_coef_polytope : c_coef polytope -> string
val string_of_c_coef_polytope' : c_coef polytope -> string
val true_c_coef_polytope : c_coef polytope
val false_c_coef_polytope : c_coef polytope
val unit_c_coef_polytope : c_coef polytope

(** data type conversion functions *)
val ineq_list_of_uminus_c_coef_polytope : c_coef polytope -> c_coef ineq list


val string_of_p_coef_polytope : p_coef polytope -> string
val true_p_coef_polytope : p_coef polytope
val false_p_coef_polytope : p_coef polytope

val ineq_list_of_uminus_p_coef_polytope : p_coef polytope -> p_coef ineq list

(** =============== *)
(** === linpred === *)
(** =============== *)
type 'coef linpred

val v_list_of_linpred : 'coef linpred -> v list

val string_of_c_coef_linpred : c_coef linpred -> string
val string_of_c_coef_linpred' : c_coef linpred -> string
val true_c_coef_linpred : c_coef linpred
val false_c_coef_linpred : c_coef linpred

val unit_c_coef_linpred : c_coef linpred

(** data type conversion functions *)
val list_of_uminus_c_coef_linpred : c_coef linpred -> c_coef polytope list

val unit_linpred : 'coef linpred
val add_polytope_to_linpred : 'coef polytope -> 'coef linpred -> 'coef linpred
val linpred_of_polytope_list : 'coef polytope list -> 'coef linpred
val linpred_of_linpred_list : 'coef linpred list -> 'coef linpred
val polytope_list_of_linpred : 'coef linpred -> 'coef polytope list



  
(* === form terms from polytope === *)
(*
val p_coef_polytope_of_p_coef_ineq_list : p_coef ineq list -> p_coef polytope
val p_coef_polytope_of_p_coef_polytope_list : p_coef polytope list -> p_coef polytope
val p_coef_ineq_list_of_p_coef_polytope : p_coef polytope -> p_coef ineq list
 *)
