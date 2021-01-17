open Util
open Variable
open Coefficient
open Polytope
open Pcfg
open Affine_term

type template = p_coef term LM.t

let var_ndet (n : int) : v = v_of_string_temporary ("ndet_" ^ string_of_int n)

(* 
  remember \eta_l (x) = a_l0 + a_l1 * x_1 + ... + a_ln * x_n
  if v = {x1, x2}, l = {l1, l2, l3},
   p = {l1, x1l1, x2l1,
        l2, x1l2, x2l2, 
        l3, x1l3, x2l3}
*)
let generate_param_list (v_domain : v_domain) (id_string: string) (l: l) : param list =
  let param_name v l = string_of_v v ^ string_of_l l ^ id_string in
  let v_list = (empty_v :: v_list_of_v_domain v_domain) in
  List.map (fun v -> param_of_string (param_name v l)) v_list

let generate_p_coef_term (v_domain : v_domain) (param_list: param list): p_coef term =
  let v_list = empty_v :: v_list_of_v_domain v_domain in
  p_coef_term_of_p_coef_v_list @@
  List.map2 (fun p v -> p_coef_of_param p, v) param_list v_list

let generate_template (l_domain : l_domain) (v_domain : v_domain) (id_string: string) : template * param_domain =
  let l_list = l_list_of_l_domain l_domain in
  let param_domain =
    param_domain_of_param_list @@ List.flatten @@ List.map (generate_param_list v_domain id_string) l_list
  in
  let eta =
    List.fold_left
    (fun eta l ->
      let param_list = generate_param_list v_domain id_string l in
      LM.add l (generate_p_coef_term v_domain param_list) eta)
    LM.empty
    l_list 
  in
  eta, param_domain

let get_pre_expectation (f: f) (template: template) : v list * (c_coef polytope * p_coef term) list =
  match f with  
  | A (assgn, l_next) ->
     let p_coef_term = LM.find l_next template in
     (match assgn with
      | Prob_assgn (v_i, distr) ->
         ([],
          [(unit_polytope,
            substitute_v_with_c_coef_term_in_p_coef_term
              (c_coef_term_of_c_coef @@ c_coef_of_float @@ expectation distr)
              v_i
              p_coef_term)])
      | Nondet_assgn linpred ->
         let (new_v_list_list, pre_expectation) =
           List.split @@ 
           List.map
             (fun polytope ->
               let v_list = remove_duplicates @@ List.filter (fun v -> not (is_empty_v v)) @@ v_list_of_polytope polytope in
               let vv_list =
                 snd @@ List.fold_left (fun (n,vv_list) v -> (n+1,(v,var_ndet n)::vv_list)) (0,[]) v_list in
               let new_c_coef_polytope =
                 List.fold_left
                   (fun polytope (v,v') ->
                     substitute_v_with_v_in_polytope polytope v v')
                   polytope
                   vv_list in
               let new_p_coef_term =
                 List.fold_left
                   (fun p_coef_term (v,v') ->
                     substitute_v_with_c_coef_term_in_p_coef_term
                       (c_coef_term_of_v v')
                       v
                       p_coef_term)
                   p_coef_term
                   vv_list in
               (List.map snd vv_list, (new_c_coef_polytope, new_p_coef_term)))
             (polytope_list_of_linpred linpred) in
         (remove_duplicates (List.flatten new_v_list_list), pre_expectation)
      | Det_assgn (v_i, lin_expr) ->
         ([],
          [(unit_polytope,
            substitute_v_with_c_coef_term_in_p_coef_term
              lin_expr
              v_i
              p_coef_term)])
     )
  | P fDist ->
     ([],
      [(unit_polytope,
        List.fold_left
          add_p_coef_term
          zero_p_coef_term
          (List.map
             (fun (l_next, prob) ->
               let p_coef_term = LM.find l_next template in
               scale_p_coef_term prob p_coef_term)
             fDist))])
  | ND guard_l_list ->
     ([],
      List.flatten @@
        List.map
          (fun (c_coef_linpred, l_next) ->
            let p_coef_term = LM.find l_next template in
            List.map
              (fun polytope -> (polytope, p_coef_term))
              (polytope_list_of_linpred c_coef_linpred)
          )
          guard_l_list)


let get_pre_condition (f: f) (template: template) : v list * (c_coef polytope * p_coef term) list =
  match f with  
  | A (assgn, l_next) ->
     let p_coef_term = LM.find l_next template in
     (match assgn with
      | Prob_assgn (v_i, distr) ->
         let linpred = get_range distr v_i in
         let (new_v_list_list, pre_expectation) =
           List.split @@ 
           List.map
             (fun polytope ->
               let v_list = remove_duplicates @@ List.filter (fun v -> not (is_empty_v v)) @@ v_list_of_polytope polytope in
               let vv_list =
                 snd @@ List.fold_left (fun (n,vv_list) v -> (n+1,(v,var_ndet n)::vv_list)) (0,[]) v_list in
               let new_c_coef_polytope =
                 List.fold_left
                   (fun polytope (v,v') ->
                     substitute_v_with_v_in_polytope polytope v v')
                   polytope
                   vv_list in
               let new_p_coef_term =
                 List.fold_left
                   (fun p_coef_term (v,v') ->
                     substitute_v_with_c_coef_term_in_p_coef_term
                       (c_coef_term_of_v v')
                       v
                       p_coef_term)
                   p_coef_term
                   vv_list in
               (List.map snd vv_list, (new_c_coef_polytope, new_p_coef_term)))
             (polytope_list_of_linpred linpred) in
         (remove_duplicates (List.flatten new_v_list_list), pre_expectation)
      | Nondet_assgn linpred ->
         let (new_v_list_list, pre_expectation) =
           List.split @@ 
           List.map
             (fun polytope ->
               let v_list = remove_duplicates @@ List.filter (fun v -> not (is_empty_v v)) @@ v_list_of_polytope polytope in
               let vv_list =
                 snd @@ List.fold_left (fun (n,vv_list) v -> (n+1,(v,var_ndet n)::vv_list)) (0,[]) v_list in
               let new_c_coef_polytope =
                 List.fold_left
                   (fun polytope (v,v') ->
                     substitute_v_with_v_in_polytope polytope v v')
                   polytope
                   vv_list in
               let new_p_coef_term =
                 List.fold_left
                   (fun p_coef_term (v,v') ->
                     substitute_v_with_c_coef_term_in_p_coef_term
                       (c_coef_term_of_v v')
                       v
                       p_coef_term)
                   p_coef_term
                   vv_list in
               (List.map snd vv_list, (new_c_coef_polytope, new_p_coef_term)))
             (polytope_list_of_linpred linpred) in
         (remove_duplicates (List.flatten new_v_list_list), pre_expectation)
      | Det_assgn (v_i, lin_expr) ->
         ([],
          [(unit_polytope,
            substitute_v_with_c_coef_term_in_p_coef_term
              lin_expr
              v_i
              p_coef_term)])
     )
  | P fDist ->
     ([],
      List.map
        (fun (l_next, _) -> (unit_polytope, LM.find l_next template))
        fDist)
  | ND guard_l_list ->
     ([],
      List.flatten @@
        List.map
          (fun (c_coef_linpred, l_next) ->
            let p_coef_term = LM.find l_next template in
            List.map
              (fun polytope -> (polytope, p_coef_term))
              (polytope_list_of_linpred c_coef_linpred)
          )
          guard_l_list)
