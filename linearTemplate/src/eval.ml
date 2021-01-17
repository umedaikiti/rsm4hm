open Util
open Variable
open Coefficient
open Affine_term
open Polytope
open Syntax
(* == open Pcfg ==
   we don't open Pcfg module since it's confssssing when mixed with Syntax module
*)

let cnt = ref (-1)
let get_new_location : unit -> Pcfg.l =
  fun _ -> cnt := !cnt + 1; Pcfg.l_of_string ("l" ^ (string_of_int !cnt))


let v_of_pvar (PVar id) : v = v_of_string id
let v_list_from_ast (ast: ast): v list =
  let v_list_from_const (const: const) : v list = [] in
  let v_list_from_dist (dist: dist) : v list = [] in
  let v_list_from_dom (dom: dom) : v list = [] in
  let rec v_list_from_expr (expr: expr) : v list = match expr with
    | EConst const -> v_list_from_const const
    | EVar pvar -> [v_of_pvar pvar]
    | EScalarMult (const, pvar) ->
      v_list_from_const const @ [v_of_pvar pvar]
    | EAdd (expr1, expr2) ->
      v_list_from_expr expr1 @ v_list_from_expr expr2
    | ESub (expr1, expr2) ->
      v_list_from_expr expr1 @ v_list_from_expr expr2
  in
  let rec v_list_from_literal (literal: literal) : v list = match literal with
    | LitLeq (expr1, expr2) ->
      v_list_from_expr expr1 @ v_list_from_expr expr2
    | LitGeq (expr1, expr2) ->
      v_list_from_expr expr1 @ v_list_from_expr expr2
    | LitNot literal ->
      v_list_from_literal literal
  in
  let rec v_list_from_affexpr (affexpr: affexpr) : v list = match affexpr with
    | AffUnit -> []
    | AffAnd (literal, affexpr) ->
      v_list_from_literal literal @ v_list_from_affexpr affexpr
  in
  let rec v_list_from_bexpr (bexpr: bexpr) : v list = match bexpr with
    | BexpUnit -> []
    | BexpOr (affexpr, bexpr) ->
      v_list_from_affexpr affexpr @ v_list_from_bexpr bexpr
  in
  let v_list_from_ndbexpr (ndbexpr: ndbexpr) : v list = match ndbexpr with
    | Ndet_branch -> []
    | Prob_branch const -> v_list_from_const const
    | Det_branch bexpr -> v_list_from_bexpr bexpr
  in
  let rec sub_v_list_from_ast (ast: ast) : v list =
    match ast with
    | StAssgn assgn -> 
      (match assgn with
       | AsnDet (pvar, expr) -> [v_of_pvar pvar] @ v_list_from_expr expr
       | AsnNdet (pvar, dom) -> [v_of_pvar pvar] @ v_list_from_dom dom
       | AsnProb (pvar, dist) -> [v_of_pvar pvar] @ v_list_from_dist dist
      )
    | StSkip -> []
    | StSeq (ast1, ast2) ->
      sub_v_list_from_ast ast1 @ sub_v_list_from_ast ast2
    | StIf (ndbexpr, ast1, ast2) ->
      v_list_from_ndbexpr ndbexpr @ sub_v_list_from_ast ast1 @ sub_v_list_from_ast ast2
    | StWhile (bexpr, ast) ->
      v_list_from_bexpr bexpr @ sub_v_list_from_ast ast
    | StAssume bexpr | StAssert bexpr ->
      v_list_from_bexpr bexpr 
  in
  remove_duplicates @@ sub_v_list_from_ast ast



let c_coef_term_of_const (Const float: const) : c_coef term =
  c_coef_term_of_c_coef @@ c_coef_of_float float
let c_coef_term_of_pvar (PVar id: pvar) : c_coef term =
  c_coef_term_of_v @@ v_of_string id
let rec c_coef_term_of_expr (expr: expr) : c_coef term = match expr with
  | EConst const -> c_coef_term_of_const const
  | EVar pvar -> c_coef_term_of_pvar pvar
  | EScalarMult (Const c, pvar) ->
    scale_c_coef_term c (c_coef_term_of_pvar pvar)
  | EAdd (expr1, expr2) ->
    add_c_coef_term (c_coef_term_of_expr expr1) (c_coef_term_of_expr expr2)
  | ESub (expr1, expr2) ->
    add_c_coef_term
      (c_coef_term_of_expr expr1)
      (uminus_c_coef_term @@ c_coef_term_of_expr expr2)

let v_assgn_of_assgn : assgn -> Pcfg.assgn = function
  | AsnDet (pvar, expr) -> Pcfg.Det_assgn (v_of_pvar pvar,  c_coef_term_of_expr expr)
  | AsnProb (pvar, (Dist (name, param_list) as dist)) ->
    (if name = "Unif" then
       (match param_list with
       | Const f1 :: Const f2 :: [] ->  (Pcfg.Prob_assgn (v_of_pvar pvar, Pcfg.Unif (f1, f2)))
       | _ -> failwith @@ "syntax error: " ^ string_of_dist dist
       )
    else if name = "Geom" then
       (match param_list with
       | Const f1 :: [] ->  (Pcfg.Prob_assgn (v_of_pvar pvar, Pcfg.Geom f1))
       | _ -> failwith @@ "syntax error: " ^ string_of_dist dist
       )
    else if name = "Norm" then
       (match param_list with
       | Const f1 :: Const f2 :: [] -> (Pcfg.Prob_assgn (v_of_pvar pvar, Pcfg.Norm (f1, f2)))
       | _ -> failwith @@ "syntax error: " ^ string_of_dist dist
       )
    else
      (** TODO: define Dist *)
      failwith @@ "undefined: " ^ name
   )
  | AsnNdet (pvar, dom) ->
    let rec linpred_of_dom : dom -> c_coef linpred = function
      | DInt -> failwith "DInt: not supported"
      | DReal -> true_c_coef_linpred
      | DIntInterval (c1, c2) -> failwith "DIntInterval: not supported"
      | DRealInterval (Const f1, Const f2) ->
         let polytope = polytope_of_ineq_list [c_coef_ineq_of_c_coef_terms (c_coef_term_of_float f1, LEQ, c_coef_term_of_v (v_of_pvar pvar)); c_coef_ineq_of_c_coef_terms (c_coef_term_of_v (v_of_pvar pvar), LEQ, c_coef_term_of_float f2)] in
         linpred_of_polytope_list [polytope]
      | DOr (d1, d2) -> linpred_of_linpred_list [linpred_of_dom d1; linpred_of_dom d2]
    in 
    Pcfg.Nondet_assgn (linpred_of_dom dom)



(*  exponential blowup in general *)
let rec not_bexpr (bexpr : bexpr) : bexpr =
  let rec not_bexpr_sub (resulting_bexpr : bexpr) (bexpr : bexpr) : bexpr =
  match bexpr with 
  | BexpUnit ->resulting_bexpr
  | BexpOr (affexpr, bexpr') -> 
     let rec cons_affexpr (resulting_bexpr' : bexpr) (affexpr : affexpr) : bexpr =
       match affexpr with
       | AffUnit -> resulting_bexpr'
       | AffAnd (literal, affexpr') ->
          let not_literal =
            match literal with 
            | LitNot literal' -> literal'
            | _ -> LitNot literal in
          let rec cons_literal (resulting_bexpr'' : bexpr) (base_bexpr'' : bexpr) : bexpr =
            match base_bexpr'' with
            | BexpUnit -> resulting_bexpr''
            | BexpOr (affexpr'', bexpr'') ->
               cons_literal (BexpOr (AffAnd (not_literal, affexpr''), resulting_bexpr'')) bexpr'' in
          cons_affexpr (cons_literal resulting_bexpr' resulting_bexpr) affexpr' in
     not_bexpr_sub (cons_affexpr BexpUnit affexpr) bexpr' in
(*  print_string ("\n" ^ string_of_bexpr bexpr ^ " -> ");
  print_string (string_of_bexpr (not_bexpr_sub (BexpOr(AffUnit,BexpUnit)) bexpr) ^ "\n"); *)
  not_bexpr_sub (BexpOr(AffUnit,BexpUnit)) bexpr


let rec (ineq_of_literal : literal -> c_coef ineq) = function
  | LitLeq (e1, e2) ->
     c_coef_ineq_of_c_coef_terms (c_coef_term_of_expr e1, LEQ, c_coef_term_of_expr e2)
  | LitGeq (e1, e2) ->
     c_coef_ineq_of_c_coef_terms (c_coef_term_of_expr e1, GEQ, c_coef_term_of_expr e2)
  | LitNot (literal) ->
     match literal with
     | LitLeq (e1, e2) -> ineq_of_literal (LitGeq (e1, e2)) (* relaxation *)
     | LitGeq (e1, e2) -> ineq_of_literal (LitLeq (e1, e2))
     | LitNot (literal) -> ineq_of_literal literal
let rec (polytope_of_affexpr : affexpr -> c_coef polytope) = function
  | AffUnit -> unit_c_coef_polytope
  | AffAnd (literal, affexpr) -> add_ineq_to_polytope (ineq_of_literal literal) (polytope_of_affexpr affexpr)
let rec (linpred_of_bexpr : bexpr -> c_coef linpred) =
  function
  | BexpUnit ->
     unit_c_coef_linpred
  | BexpOr (affexpr, bexpr) ->
     add_polytope_to_linpred (polytope_of_affexpr affexpr) (linpred_of_bexpr bexpr)

(*
let add_e_list_to_e_map (Pcfg.l l) (edge list : e_l) (edge list Pcfg.LM.t : e_map) : edge list Pcfg.LM.t =
  try
    Pcfg.LM.add l (List.rev_append e_l (Pcfg.LM.find l e_list)) e_map
  with 
  | Not_found -> Pcfg.LM.add l e_l e_map
 *)

let rec pcfg_of_ast (ast: ast) (l_term: Pcfg.l) : (Pcfg.l * Pcfg.pcfg_transition * Pcfg.linpredmap * Pcfg.linpredmap * Pcfg.rewardmap * Pcfg.l list) =
  match ast with 
  | StAssgn assgn ->
     let a = v_assgn_of_assgn assgn in
     let l = get_new_location () in
     (l,
      Pcfg.pcfg_transition_singleton l (Pcfg.A (a,l_term)),
      Pcfg.linpredmap_empty,
      Pcfg.linpredmap_empty,
      Pcfg.rewardmap_singleton l 1.,
      [l])
  | StSkip ->
     let l = get_new_location () in
     (l,
      Pcfg.pcfg_transition_singleton  l (Pcfg.P (Pcfg.fDist_of_list [(l_term, 1.)])),
      Pcfg.linpredmap_empty,
      Pcfg.linpredmap_empty,
      Pcfg.rewardmap_singleton l 1.,
      [l])
  | StSeq (s1, s2) ->
     let l2, pcfg_transition2, invmap2, termmap2, rewardmap2, l_list2 = pcfg_of_ast s2 l_term in
     let l1, pcfg_transition1, invmap1, termmap1, rewardmap1, l_list1 = pcfg_of_ast s1 l2 in
     (l1,
      Pcfg.pcfg_transition_fold_add pcfg_transition1 pcfg_transition2,
      Pcfg.linpredmap_fold_add invmap1 invmap2,
      Pcfg.linpredmap_fold_add termmap1 termmap2,
      Pcfg.rewardmap_fold_add rewardmap1 rewardmap2,
      l_list1@l_list2)
  | StIf (ndbexpr, s1, s2) -> 
     let l = get_new_location () in
     let (l1, pcfg_transition1, invmap1, termmap1, rewardmap1, l_list1), (l2, pcfg_transition2, invmap2, termmap2, rewardmap2, l_list2) =
       pcfg_of_ast s1 l_term, pcfg_of_ast s2 l_term in
     let pcfg_transition = Pcfg.pcfg_transition_fold_add pcfg_transition1 pcfg_transition2 in
     let invmap' = Pcfg.linpredmap_fold_add invmap1 invmap2 in
     let termmap' = Pcfg.linpredmap_fold_add termmap1 termmap2 in
     let rewardmap = Pcfg.rewardmap_fold_add rewardmap1 rewardmap2 in
     let l_list = l::(l_list1@l_list2) in
     (match ndbexpr with
      | Ndet_branch ->
         (l,
          Pcfg.pcfg_transition_add l (Pcfg.ND [(true_c_coef_linpred, l1); (true_c_coef_linpred, l2)]) pcfg_transition,
          invmap',
          termmap',
          Pcfg.rewardmap_add l 1. rewardmap,
          l_list)
      | Prob_branch (Const f) ->
         (l,
          Pcfg.pcfg_transition_add l (Pcfg.P (Pcfg.fDist_of_list [(l1,f); (l2,1.-.f)])) pcfg_transition,
          invmap',
          termmap',
          Pcfg.rewardmap_add l 1. rewardmap,
          l_list)
      | Det_branch bexpr ->
         (l,
          Pcfg.pcfg_transition_add l (Pcfg.ND [(linpred_of_bexpr bexpr, l1); (linpred_of_bexpr @@ not_bexpr bexpr, l2)]) pcfg_transition,
          invmap',
          termmap',
          Pcfg.rewardmap_add l 1. rewardmap,
          l_list))
  | StWhile (bexpr, ast) ->
     let l = get_new_location () in
     let (l1,pcfg_transition,invmap,termmap,rewardmap,l_list) = pcfg_of_ast ast l in
     (l,
      Pcfg.pcfg_transition_add l (Pcfg.ND [(linpred_of_bexpr bexpr, l1); (linpred_of_bexpr @@ not_bexpr bexpr, l_term)]) pcfg_transition,
      invmap,
      termmap,
      Pcfg.rewardmap_add l 1. rewardmap,
      l::l_list)
  | StAssume bexpr ->
     (l_term,
      Pcfg.pcfg_transition_empty,
      Pcfg.linpredmap_singleton l_term (linpred_of_bexpr bexpr),
      Pcfg.linpredmap_empty,
      Pcfg.rewardmap_empty,
      [])
  | StAssert bexpr -> 
     let l = get_new_location () in
     (l,
      Pcfg.pcfg_transition_singleton  l (Pcfg.P (Pcfg.fDist_of_list [(l_term, 1.)])),
      Pcfg.linpredmap_empty,
      Pcfg.linpredmap_singleton l (linpred_of_bexpr bexpr),
      Pcfg.rewardmap_singleton l 1.,
      [l])
    

let eval (Prog (bexprg, ast) : prog) : Pcfg.pcfg * Pcfg.config * Pcfg.linpredmap * Pcfg.linpredmap * Pcfg.rewardmap  =
  let glbinv = linpred_of_bexpr bexprg in
  let l_term = Pcfg.l_of_string "l_exit" in
  let (l_init, pcfg_transition, invariant, term_linpredmap, rewardmap, l_list) = pcfg_of_ast ast l_term in
  let pcfg_transition = Pcfg.pcfg_transition_add l_term (Pcfg.P (Pcfg.fDist_of_list [(l_term, 1.)])) pcfg_transition in
  let rewardmap = Pcfg.rewardmap_add l_term 0. rewardmap in
  let l_list = l_term::l_list in
  let invariant =
    List.fold_left
      (fun invariant l ->
        if Pcfg.linpredmap_mem l invariant then invariant else
        Pcfg.linpredmap_add l glbinv invariant)
      invariant
      l_list in
  let v_list = v_list_from_ast ast in
  let init_config : Pcfg.config = (l_init, valuation_of_var_float_list @@ List.map (fun x -> (x,0.)) v_list) in (* initial configuration: \forall x. x=0 *)
(*  print_string @@ string_of_list Pcfg.string_of_l l_list;
  print_string @@ string_of_list (fun (l, f) -> Pcfg.string_of_l l ^ " " ^ Pcfg.string_of_f f) (Pcfg.LM.bindings pcfg_transition);
  print_string @@ "invariant: "; 
  print_string @@ Pcfg.string_of_linpredmap invariant;
  print_string @@ "terminal configuration: "; 
  print_string @@ Pcfg.string_of_linpredmap term_linpredmap ^ "\n" ; *)
  ((pcfg_transition, Pcfg.l_domain_of_l_list l_list, v_domain_of_v_list v_list), init_config, invariant, term_linpredmap, rewardmap)
