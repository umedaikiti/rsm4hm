open Variable
open Coefficient
open Polynomial
open Semialg
open Pcfg

type template = p_coef polynomial LM.t


val sequents_and_objective_func_of_reachability_problem : reachability_problem -> template list -> (string * (and_ineq_list * p_coef ineq) list) list * p_coef * v list
