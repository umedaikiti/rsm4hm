(** module Affine_term:
    this module handles affine terms and affine expressions with usual variables and possibly with another kind of variables.
    terms form an expression as an inequality or a polytope.
    this module also defines vector and matrix
*)

open Variable
open Coefficient

(** ============================= *)
(** === term over coefficient === *)
(** ============================= *)
type 'coef term

(** -------------- *)
(** 1. c_coef term
   \sum_i a_i x_i
 *)
val normalize_c_coef_term : c_coef term -> c_coef term
val zero_c_coef_term : c_coef term
val unit_c_coef_term : c_coef term
val string_of_c_coef_term : c_coef term -> string
val string_of_c_coef_term' : c_coef term -> string

(** data type conversion functions *)
val c_coef_term_of_float : float -> c_coef term
val c_coef_term_of_c_coef : c_coef -> c_coef term
val c_coef_term_of_v : v -> c_coef term
val c_coef_term_of_c_coef_v_list : (c_coef * v) list -> c_coef term
val c_coef_v_list_of_c_coef_term : c_coef term -> (c_coef * v) list
val c_coef_list_of_c_coef_term : c_coef term -> c_coef list
val v_list_of_c_coef_term : c_coef term -> v list
val v_list_of_term : 'coef term -> v list

(** operations on data type *)
(* == for affine term == 
   i.e., no multiplication between affine terms
   you should use scale_* to multiply float
*)
val add_c_coef_term : c_coef term -> c_coef term -> c_coef term
val uminus_c_coef_term : c_coef term -> c_coef term
val scale_c_coef_term : float -> c_coef term -> c_coef term

(** ----------------- *)
(** == p_coef term ==
   \sum_i (\sum_j a_j p_j) x_i
 *)
val normalize_p_coef_term : p_coef term -> p_coef term
val zero_p_coef_term : p_coef term
val unit_p_coef_term : p_coef term
val string_of_p_coef_term : p_coef term -> string
val string_of_p_coef_term' : p_coef term -> string

(** data type conversion functions *)
val p_coef_term_of_float : float -> p_coef term
val p_coef_term_of_p_coef : p_coef -> p_coef term
val p_coef_term_of_v : v -> p_coef term
val p_coef_term_of_p_coef_v_list : (p_coef * v) list -> p_coef term
val p_coef_v_list_of_p_coef_term : p_coef term -> (p_coef * v) list
val p_coef_list_of_p_coef_term : p_coef term -> p_coef list
val v_list_of_p_coef_term : p_coef term -> v list


(** operations on data type *)
(* == for affine term == 
   i.e., no multiplication between affine terms
   you should use scale_* to multiply float
*)
val add_p_coef_term : p_coef term -> p_coef term -> p_coef term
val uminus_p_coef_term : p_coef term -> p_coef term
val scale_p_coef_term : float -> p_coef term -> p_coef term


(** other utility functions *)
val substitute_v_with_v_in_coef_term : 'coef term -> v -> v -> 'coef term
val substitute_v_with_c_coef_term_in_p_coef_term : c_coef term -> v -> p_coef term -> p_coef term
