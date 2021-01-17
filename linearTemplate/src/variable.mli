(** ================= *)
(** === variables === *)
(** ================= *)
type 'var valuation
val value_of : 'a -> 'a valuation -> float
val update_of_valuation : 'var valuation -> 'var -> float -> 'var valuation
val valuation_of_var_float_list : ('var * float) list -> 'var valuation
val var_float_list_of_valuation : 'var valuation -> ('var * float) list
(* val string_of_valuation : 'var valuation -> string *)

(** ----------- *)
(** 1. variable *)
(* e.g. x_i *)
type v
val v_of_string : string -> v
val v_of_string_temporary : string -> v
val empty_v : v
val string_of_v : v -> string
val compare_v : v -> v -> int

(** data type conversion functions *)
val v_of_string : string -> v

val is_empty_v : v -> bool

val normalize_v : v -> v

(** --------------------- *)
(** 1.1. set of variables *)
type v_domain
val v_domain_of_v_list : v list -> v_domain
val v_list_of_v_domain : v_domain -> v list
val string_of_v_domain : v_domain -> string

(** ----------------------------------- *)
(** 2. parameter (or decision variable) *)
(* e.g. p_i *)
type param
val param_of_string : string -> param
val empty_param : param
val string_of_param : param -> string
val compare_param : param -> param -> int

(** data type conversion functions *)
val param_of_string : string -> param

(** ---------------------- *)
(** 2.1. set of parameters *)
type param_domain
val param_domain_of_param_list : param list -> param_domain
val param_list_of_param_domain : param_domain -> param list
val string_of_param_domain : param_domain -> string

val is_empty_param : param -> bool
