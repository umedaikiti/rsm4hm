open Util
open Variable
open Coefficient
open Pcfg
open Polynomial
open Semialg


type template = p_coef polynomial LM.t


let get_objective_func (template: template) (l_init: l) (x_init: v valuation) (v_domain: v_domain) : p_coef =
  let template_l_init = LM.find l_init template in
  (* \eta_l_init (\vec(x)_init)*)
  List.fold_left
    (fun current_p_coef (p_coef, monomial) ->
      add_p_coef
        current_p_coef
        (mult_float_with_p_coef (valuate_monomial x_init monomial) p_coef))
    zero_p_coef
    (p_coef_monomial_list_of_p_coef_polynomial template_l_init)


let var_ndet (n : int) : v = v_of_string_temporary ("ndet_" ^ string_of_int n)

let get_pre_expectation (f: f) (template: template) : v list * (and_ineq_list * p_coef polynomial) list =
  match f with  
  | A (assgn, l_next) ->
     let p_coef_polynomial = LM.find l_next template in
     (match assgn with
      | Prob_assgn (v_i, distr) ->
         ([],
          [(true_and_ineq_list,
            substitute_vn_with_float_in_p_coef_polynomial
              v_i
              (raw_moment distr)
              p_coef_polynomial 
         )])
      | Nondet_assgn semialg ->
         let (new_v_list_list, pre_expectation) =
           List.split @@ 
             List.map
               (fun and_ineq_list ->
                 let v_list = remove_duplicates @@ List.filter (fun v -> not (is_empty_v v)) @@ v_list_of_and_ineq_list and_ineq_list in
                 let vv_list =
                   snd @@ List.fold_left (fun (n,vv_list) v -> (n+1,(v,var_ndet n)::vv_list)) (0,[]) v_list in
                 let new_c_coef_and_ineq_list =
                   List.fold_left
                     (fun and_ineq_list (v,v') ->
                       substitute_v_with_v_in_and_ineq_list
                         v
                         v'
                         and_ineq_list)
                     and_ineq_list
                     vv_list in
                 let new_p_coef_polynomial =
                   List.fold_left
                     (fun p_coef_polynomial (v,v') ->
                       substitute_v_with_v_in_p_coef_polynomial
                         v
                         v'
                         p_coef_polynomial)
                     p_coef_polynomial
                     vv_list in
                 (List.map snd vv_list, (new_c_coef_and_ineq_list, new_p_coef_polynomial)))
               (and_ineq_list_list_of_semialg semialg) in
         (remove_duplicates (List.flatten new_v_list_list), pre_expectation)
      | Det_assgn (v_i, c_coef_polynomial) ->
         ([],
          [(true_and_ineq_list,
            substitute_v_with_c_coef_polynomial_in_p_coef_polynomial
              v_i
              c_coef_polynomial
              p_coef_polynomial)])
     )
  | P fDist ->
     ([],
      [(true_and_ineq_list,
        List.fold_left
          add_p_coef_polynomial
          zero_p_coef_polynomial
          (List.map
             (fun (l_next, prob) ->
               let p_coef_polynomial = LM.find l_next template in
               mult_float_with_p_coef_polynomial prob p_coef_polynomial)
             fDist))])
  | ND guard_l_list ->
     ([],
      List.flatten @@
        List.map
          (fun (c_coef_semialg, l_next) ->
            let p_coef_polynomial = LM.find l_next template in
            List.map
              (fun and_ineq_list -> (and_ineq_list, p_coef_polynomial))
              (and_ineq_list_list_of_semialg c_coef_semialg)
          )
          guard_l_list) 




let get_sm_const (pcfg_transition: pcfg_transition) (term_config: semialgmap) (invmap: semialgmap) (rewardmap : rewardmap) (template: template) (template_elapse: float -> template) (l: l) : v list * (and_ineq_list * p_coef ineq) list =
  if LM.mem l pcfg_transition then
    let f = LM.find l pcfg_transition in
    let template_l = LM.find l template in
    let reward_l = LM.find l rewardmap in
    let inv_and_ineq_list_list =
      and_ineq_list_list_of_semialg (try LM.find l invmap with Not_found -> true_semialg) in
    let term_and_ineq_list =
      if LM.mem l term_config then
        match and_ineq_list_list_of_semialg @@ LM.find l term_config with
        | [] -> false_and_ineq_list
        | term_and_ineq_list::[] -> term_and_ineq_list 
        | _ -> failwith "Terminal configuration should be a conjunctive formula."
      else
        false_and_ineq_list in
    let (v_list, pre_expectations) : v list * (and_ineq_list * p_coef polynomial) list = get_pre_expectation f (template_elapse reward_l) in
    (v_list,
     List.flatten @@
     List.map
       (fun negated_ineq ->
         List.flatten @@
         List.map
           (fun (guard_polytope, pre_expectation_p_coef) ->
             List.map
               (fun inv_polytope -> 
                 (and_ineq_list_of_and_ineq_list_list [and_ineq_list_of_c_coef_ineq_list [negated_ineq]; guard_polytope; inv_polytope],
                  p_coef_ineq_of_p_coef_polynomials (template_l, GEQ, pre_expectation_p_coef)))
               inv_and_ineq_list_list)
           pre_expectations)
       (List.map not_c_coef_ineq @@ c_coef_ineq_list_of_and_ineq_list term_and_ineq_list))
  else failwith "error: not_found in get_sm_const"

let get_nonneg_const (invmap:semialgmap) (template: template) (l: l) :(and_ineq_list * p_coef ineq) list =
  let template_l = LM.find l template in
  let inv_and_ineq_list_list =
    and_ineq_list_list_of_semialg (try LM.find l invmap with Not_found -> true_semialg) in
  List.map
    (fun inv_and_ineq_list -> (inv_and_ineq_list, p_coef_ineq_of_p_coef_polynomials (template_l, GEQ, p_coef_polynomial_of_p_coef @@ p_coef_of_float 0.)))
    inv_and_ineq_list_list

let rec combination (n: int) (k: int) : float =
  if k < 0 || n < 0 || n < k then failwith "error: invalid argument in combination"
  else if k = 0 then 1. else float_of_int n /. float_of_int k *. combination (n-1) (k-1)

let elapse_p_coef_polynomial (a: float) (p_coef_poly_list: p_coef polynomial list) =
  let n = List.length p_coef_poly_list in
  let elapse_poly_list = (p_coef_polynomial_of_p_coef @@ p_coef_of_float (a ** float_of_int n)) :: List.mapi (fun i p -> mult_float_with_p_coef_polynomial ((a ** float_of_int i) *. combination n i) p) p_coef_poly_list in
  add_p_coef_polynomial_list elapse_poly_list

let elapse_template (template_list: template list) (l_list: l list) (a : float) : template = List.fold_left (fun eta l -> LM.add l (elapse_p_coef_polynomial a @@ List.map (LM.find l) template_list) eta) LM.empty l_list

(** a compiler based on the theory developed in ??? *)
let sequents_and_objective_func_of_reachability_problem ((pcfg_transition, l_domain, v_domain), (l_init,init_valuation), invariant, term_config, rewardmap, order: reachability_problem) (template_list: template list) : (string * (and_ineq_list * p_coef ineq) list) list * p_coef * v list =
  let rec main_loop i consts new_vars template_list' =
    if i <= order then
      let template = List.nth template_list (i-1) in
      let template_list' = template :: template_list' in
      let l_list = l_list_of_l_domain l_domain in
      let new_vars', sm_const_output =
        (fun (x,y) -> (List.flatten x,List.flatten y)) @@
        List.split @@
        List.map
          (get_sm_const pcfg_transition term_config invariant rewardmap template (elapse_template template_list' l_list))
          l_list in
      let nonneg_const_output =
        List.flatten @@
        List.map
          (get_nonneg_const invariant template)
          l_list in
      main_loop (i+1) (consts @ [("nonnegative constraints " ^ string_of_int i, nonneg_const_output); ("supermartingale constraints " ^ string_of_int i, sm_const_output)]) (new_vars @ new_vars') template_list'
    else (consts, new_vars) in
  let consts, new_vars = main_loop 1 [] [] [] in
(*  List.iter (fun (s, l) -> print_endline s;
    List.iter (fun (a, p) ->
        print_endline @@ string_of_and_ineq_list a;
        print_endline @@ string_of_p_coef_ineq p
    ) l) consts;*)
  let objective_func = get_objective_func (List.nth template_list (order-1)) l_init init_valuation v_domain in
  (consts, objective_func, new_vars)
