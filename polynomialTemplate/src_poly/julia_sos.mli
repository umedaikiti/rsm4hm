open Coefficient
open Polynomial
open Pcfg

type julia_sos = string

val julia_sos_of_reachability_problem : reachability_problem -> int -> int -> string -> julia_sos
