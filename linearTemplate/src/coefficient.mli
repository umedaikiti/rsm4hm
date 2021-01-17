open Variable

(** =================== *)
(** === coefficient === *)
(** =================== *)

(** --------- *)
(** 1. c_coef *)
(* e.g. c_i *)
type c_coef
val zero_c_coef : c_coef
val unit_c_coef : c_coef
val string_of_c_coef : c_coef -> string
val string_of_c_coef' : c_coef -> string

(** data type conversion functions *)
val c_coef_of_float : float -> c_coef
val float_of_c_coef : c_coef -> float

(** operations on data type *)
val add_c_coef : c_coef -> c_coef -> c_coef
val uminus_c_coef : c_coef -> c_coef
val mult_c_coef : c_coef -> c_coef -> c_coef
val div_c_coef : c_coef -> c_coef -> c_coef

(** --------- *)
(** 2. p_coef *)
(* e.g. \sum_i c_i p_i *)
type p_coef
val zero_p_coef : p_coef
val unit_p_coef : p_coef
val string_of_p_coef : p_coef -> string
val string_of_p_coef' : p_coef -> string

(** data type conversion functions *)
val p_coef_of_float : float -> p_coef
val p_coef_of_param : param -> p_coef
val p_coef_of_float_param : float * param -> p_coef
val p_coef_of_float_param_list : (float * param) list -> p_coef
val p_coef_of_c_coef : c_coef -> p_coef
val float_param_list_of_p_coef : p_coef -> (float * param) list

val mult_float_with_p_coef : float -> p_coef -> p_coef

(** operations on data type *)
val add_p_coef : p_coef -> p_coef -> p_coef
val uminus_p_coef : p_coef -> p_coef

val scale_p_coef : float -> p_coef -> p_coef
