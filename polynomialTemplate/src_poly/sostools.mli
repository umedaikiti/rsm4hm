open Coefficient
open Polynomial
open Pcfg

type sostools = string

val sostools_of_reachability_problem : reachability_problem -> int -> int -> string -> sostools
