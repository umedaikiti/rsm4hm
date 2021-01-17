open Util
open Variable
open Coefficient
open Affine_term
open Polytope
open Matrix
open Pcfg
open Supermartingale

type lp_constraint = (c_coef matrix * c_coef vector) * (p_coef vector * p_coef)

(*
let negate_polytope_and_map (f : c_coef ineq -> 'a list) (polytope: c_coef polytope)  : 'a list =
  List.flatten (List.map (fun c_coef_ineq -> f (uminus_c_coef_ineq c_coef_ineq)) (ineq_list_of_uminus_c_coef_polytope polytope))
 *)

let string_of_output_elm =
  (string_of_pair (string_of_pair (string_of_matrix string_of_c_coef) (string_of_vector (string_of_c_coef))) (string_of_pair (string_of_vector string_of_p_coef) string_of_p_coef))

let string_of_output = string_of_list (string_of_pair (string_of_pair (string_of_matrix string_of_c_coef) (string_of_vector (string_of_c_coef))) (string_of_pair (string_of_vector string_of_p_coef) string_of_p_coef))

(*
let get_sm_const (pcfg_transition: pcfg_transition) (epsilon: float) (term_config: linpredmap) (invmap: linpredmap) (template: template) (l: l) : v list * (c_coef polytope * p_coef ineq) list =
  if LM.mem l pcfg_transition then
    let f = LM.find l pcfg_transition in
    let template_l = LM.find l template in
    let inv_polytope_list =
      polytope_list_of_linpred (try LM.find l invmap with Not_found -> true_c_coef_linpred) in
    let term_polytope =
      if LM.mem l term_config then
        match polytope_list_of_linpred @@ LM.find l term_config with
        | [] -> false_c_coef_polytope
        | term_polytope::[] -> term_polytope 
        | _ -> failwith "Terminal configuration should be a conjunctive formula."
      else
        false_c_coef_polytope in
    let (v_list, pre_expectations) : v list * (c_coef polytope * p_coef term) list = get_pre_expectation f template in
    (v_list,
     List.flatten @@
     List.map
       (fun negated_ineq ->
         List.flatten @@
         List.map
           (fun (guard_polytope, pre_expectation_p_coef) ->
             List.map
               (fun inv_polytope -> 
                 (polytope_of_polytope_list [polytope_of_ineq_list [negated_ineq]; guard_polytope; inv_polytope],
                  p_coef_ineq_of_p_coef_terms (template_l, GEQ, add_p_coef_term (p_coef_term_of_float epsilon) pre_expectation_p_coef)))
               inv_polytope_list)
           pre_expectations)
       (ineq_list_of_uminus_c_coef_polytope term_polytope))
  else failwith "error: not_found in get_sm_const"
*)

(*
let get_bdddiff_const (pcfg_transition: pcfg_transition) (kappa: float) (term_config: linpredmap) (invmap: linpredmap) (template: template) (l: l) : v list * (c_coef polytope * p_coef ineq) list =
  if LM.mem l pcfg_transition then
    let f = LM.find l pcfg_transition in
    let template_l = LM.find l template in
    let inv_polytope_list =
      polytope_list_of_linpred (try LM.find l invmap with Not_found -> true_c_coef_linpred) in
    let term_polytope =
      if LM.mem l term_config then
        match polytope_list_of_linpred @@ LM.find l term_config with
        | [] -> false_c_coef_polytope
        | term_polytope::[] -> term_polytope 
        | _ -> failwith "Terminal configuration should be a conjunctive formula."
      else
        false_c_coef_polytope in
    let (v_list, pre_expectations) : v list * (c_coef polytope * p_coef term) list = get_pre_condition f template in
    (v_list,
     List.flatten @@
     List.map
       (fun negated_ineq ->
         List.flatten @@
         List.map
           (fun (guard_polytope, pre_expectation_p_coef) ->
             List.flatten @@
             List.map
               (fun inv_polytope -> 
                 [(polytope_of_polytope_list [polytope_of_ineq_list [negated_ineq]; guard_polytope; inv_polytope],
                  p_coef_ineq_of_p_coef_terms (template_l, LEQ, add_p_coef_term (p_coef_term_of_float (kappa)) pre_expectation_p_coef));
                  (polytope_of_polytope_list [polytope_of_ineq_list [negated_ineq]; guard_polytope; inv_polytope],
                  p_coef_ineq_of_p_coef_terms (template_l, GEQ, add_p_coef_term (p_coef_term_of_float (-.kappa)) pre_expectation_p_coef))
               ])
               inv_polytope_list)
           pre_expectations)
       (ineq_list_of_uminus_c_coef_polytope term_polytope))
  else failwith "error: not_found in get_sm_const"




let get_scaling_const (invmap: linpredmap) (term_config: linpredmap) (template: template) (l: l) : (c_coef polytope * p_coef ineq) list =
  let template_l = LM.find l template in
  let inv_polytope_list =
    polytope_list_of_linpred (try LM.find l invmap with Not_found -> true_c_coef_linpred) in
  let term_polytope =
    if LM.mem l term_config then
      match polytope_list_of_linpred @@ LM.find l term_config with
      | [] -> false_c_coef_polytope
      | term_polytope::[] -> term_polytope 
      | _ -> failwith "Terminal configuration should be a conjunctive formula."
    else
      false_c_coef_polytope in
  List.map
    (fun inv_polytope -> 
      (polytope_of_polytope_list [term_polytope;inv_polytope],
       p_coef_ineq_of_p_coef_terms (template_l, GEQ, p_coef_term_of_p_coef @@ p_coef_of_float 0.)))
    inv_polytope_list
*)


let put_plus lst = string_of_list_infix_string (fun x -> x) lst "+"
let put_times lst = string_of_list_infix_string (fun x -> x) lst "*"

let obj_func_of_float f =
  "(" ^ string_of_float f ^ "0" ^ ")"

let obj_func_of_p_coef (p_coef : p_coef) : string =
  let obj_func_of_float_param (f, p) =
    if is_empty_param p then obj_func_of_float f
    else obj_func_of_float f ^ "*" ^ string_of_param p
  in
  let float_param_list = float_param_list_of_p_coef p_coef in
  "(" ^ string_of_list_infix_string obj_func_of_float_param float_param_list "+" ^ ")"

let get_objective_func (template: template) (l_init: l) (x_init: v valuation) (v_domain: v_domain) : p_coef =
  let template_l_init = LM.find l_init template in
  (* \eta_l_init (\vec(x)_init)*)
    List.fold_left
    (fun current_p_coef (p_coef, v) ->
      let valuation_v =
        if is_empty_v v then 1. else
        try value_of v x_init with Not_found -> failwith ("Not_found in get_objective_func: " ^ string_of_v v ^ ".") in
      add_p_coef (mult_float_with_p_coef valuation_v p_coef) current_p_coef)
    zero_p_coef
    (p_coef_v_list_of_p_coef_term template_l_init)

let get_nonneg_const (invmap: linpredmap) (template: template) (l: l) : (c_coef polytope * p_coef ineq) list =
  let template_l = LM.find l template in
  let inv_polytope_list =
    polytope_list_of_linpred (try LM.find l invmap with Not_found -> true_c_coef_linpred) in
  List.map
    (fun inv_polytope -> 
      (inv_polytope,
       p_coef_ineq_of_p_coef_terms (template_l, GEQ, p_coef_term_of_p_coef @@ p_coef_of_float 0.)))
    inv_polytope_list

let rec combination (n: int) (k: int) : float =
  if k < 0 || n < 0 || n < k then failwith "error: invalid argument in combination"
  else if k = 0 then 1. else float_of_int n /. float_of_int k *. combination (n-1) (k-1)

let elapse_p_coef_term (a: float) (p_coef_term_list: p_coef term list) =
  let n = List.length p_coef_term_list in
  let _, elapse_term = List.fold_left (fun (i, t) t_i -> (i-1, add_p_coef_term t @@ scale_p_coef_term ((a ** float_of_int (n-i)) *. combination n i) t_i)) (n, p_coef_term_of_p_coef @@ p_coef_of_float (a ** float_of_int n)) p_coef_term_list in
  elapse_term

let elapse_template (template_list: template list) l_domain (a : float) : template = List.fold_left (fun eta l -> LM.add l (elapse_p_coef_term a @@ List.map (LM.find l) template_list) eta) LM.empty l_domain

let get_sm_const (pcfg_transition: pcfg_transition) (term_config: linpredmap) (invmap: linpredmap) (template: template) (elapse: float -> template) (rewardmap: rewardmap) (l: l) : v list * (c_coef polytope * p_coef ineq) list =
  if LM.mem l pcfg_transition then
    let f = LM.find l pcfg_transition in
    let template_l = LM.find l template in
    let reward_l = LM.find l rewardmap in
    let inv_polytope_list =
      polytope_list_of_linpred (try LM.find l invmap with Not_found -> true_c_coef_linpred) in
    let term_polytope =
      if LM.mem l term_config then
        match polytope_list_of_linpred @@ LM.find l term_config with
        | [] -> false_c_coef_polytope
        | term_polytope::[] -> term_polytope 
        | _ -> failwith "Terminal configuration should be a conjunctive formula."
      else
        false_c_coef_polytope in
    let (v_list, pre_expectations) : v list * (c_coef polytope * p_coef term) list = get_pre_expectation f (elapse reward_l) in
    (v_list,
     List.flatten @@
     List.map
       (fun negated_ineq ->
         List.flatten @@
         List.map
           (fun (guard_polytope, pre_expectation_p_coef) ->
             List.map
               (fun inv_polytope -> 
                 (polytope_of_polytope_list [polytope_of_ineq_list [negated_ineq]; guard_polytope; inv_polytope],
                  p_coef_ineq_of_p_coef_terms (template_l, GEQ, pre_expectation_p_coef)))
               inv_polytope_list)
           pre_expectations)
       (ineq_list_of_uminus_c_coef_polytope term_polytope))
  else failwith "error: not_found in get_sm_const"

let string_of_const ((a, b): c_coef polytope * p_coef ineq) = string_of_c_coef_polytope' a ^ " -> " ^ string_of_p_coef_ineq' b

(** a compiler based on the theory developed in CAV2018 *)
let lp_seeds_of_reachability_problem ((pcfg_transition, l_domain, v_domain), order, (l_init,init_valuation), invariant, term_config, rewardmap: reachability_problem) debug : lp_constraint list * string list * string =
  (*List.fold_left*)
  let rec lp_seeds_of_reachability_problem' i template_list const_output template_var_list new_vars =
    if i <= order then
      let template, template_var_domain = generate_template l_domain v_domain ("o" ^ string_of_int i) in
      let nonneg_const_output =
        List.flatten @@ List.map
          (get_nonneg_const invariant template)
          (l_list_of_l_domain l_domain) in
      let new_vars', sm_const_output = 
        (fun (x,y) -> (List.flatten x,List.flatten y)) @@
        List.split @@
        List.map
        (get_sm_const pcfg_transition term_config invariant template (elapse_template (template::template_list) (l_list_of_l_domain l_domain)) rewardmap)
        (l_list_of_l_domain l_domain) in
      lp_seeds_of_reachability_problem' (i+1) (template::template_list) (const_output @ nonneg_const_output @ sm_const_output) (template_var_list @ param_list_of_param_domain template_var_domain) (new_vars @ new_vars')
    else
     let objective_func_string = obj_func_of_p_coef
       (get_objective_func (List.hd template_list) l_init init_valuation v_domain) in
        (const_output, template_var_list, objective_func_string, new_vars) in
  let const_output, template_var_list, objective_func_string, new_vars = lp_seeds_of_reachability_problem' 1 [] [] [] [] in
  if debug then List.iter (fun const -> print_endline @@ string_of_const const) const_output else ();
  let v_list_order_fixed : v list =
    remove_duplicates @@
    List.filter (fun v -> not (is_empty_v v)) @@
    List.map normalize_v (new_vars @ (v_list_of_v_domain v_domain)) in
  let lp_constraints : lp_constraint list =
    List.map
      (fun (c_coef_polytope, p_coef_ineq) ->
        (split_c_coef_polytope v_list_order_fixed c_coef_polytope,
         split_p_coef_ineq v_list_order_fixed p_coef_ineq))
      const_output in
  (lp_constraints,
    List.map string_of_param template_var_list,
    objective_func_string)

