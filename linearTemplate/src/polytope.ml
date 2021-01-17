open Util
open Variable
open Coefficient
open Affine_term

type 'coef scalar = 'coef
(* let string_of_scalar *)
let coef_of_scalar x = x
let is_c_coef_scalar (_, v: c_coef * v) : bool = v = empty_v
let is_p_coef_scalar (_, v: p_coef * v) : bool = v = empty_v

(* === inequality, polytope over coefficient === *)
(* inequality, polytope cannot be minused due to its semantics *)
(* === form terms from inequality, polytope === *)
type bop = LEQ | GEQ
let string_of_bop = function | LEQ -> "LEQ" | GEQ -> "GEQ"

(* ineq *)
type 'coef ineq = 'coef term

let substitute_v_with_v_in_ineq = substitute_v_with_v_in_coef_term

let v_list_of_ineq = v_list_of_term
  


let string_of_c_coef_ineq (ineq: c_coef ineq) : string =
  "(ineq: " ^ string_of_c_coef_term ineq ^ ")"
let string_of_c_coef_ineq' (ineq: c_coef ineq) : string =
  string_of_c_coef_term' ineq ^ " <= 0"
let false_c_coef_ineq : c_coef ineq = unit_c_coef_term (* 1 <= 0*)
let uminus_c_coef_ineq = uminus_c_coef_term
let true_c_coef_ineq : c_coef ineq =  uminus_c_coef_ineq false_c_coef_ineq 


let c_coef_ineq_of_c_coef_terms (t1, bop, t2 : c_coef term * bop * c_coef term): c_coef ineq =
  match bop with
    LEQ -> add_c_coef_term t1 (uminus_c_coef_term t2)
  | GEQ -> add_c_coef_term (uminus_c_coef_term t1) t2
let c_coef_term_of_c_coef_ineq x = x

let c_coef_scalar_of_c_coef_ineq ineq =
  match List.filter is_c_coef_scalar (c_coef_v_list_of_c_coef_term @@ c_coef_term_of_c_coef_ineq ineq) with
  | [] -> zero_c_coef
  | hd :: _ -> uminus_c_coef @@ fst hd

let wo_c_coef_scalar_of_c_coef_ineq v_list ineq =
  let term = normalize_c_coef_term ineq in
  List.map
    (fun v ->
      try
        fst @@ List.find (fun (_, v') -> v' = v) (c_coef_v_list_of_c_coef_term term)
      with Not_found -> zero_c_coef)
    v_list


let string_of_p_coef_ineq (ineq: p_coef ineq) : string =
  "(ineq: " ^ string_of_p_coef_term ineq ^ ")"
let string_of_p_coef_ineq' (ineq: p_coef ineq) : string =
  string_of_p_coef_term' ineq ^ " <= 0"
let false_p_coef_ineq : p_coef ineq = unit_p_coef_term (* 1 <= 0*)
let uminus_p_coef_ineq = uminus_p_coef_term
let true_p_coef_ineq : p_coef ineq = uminus_p_coef_ineq false_p_coef_ineq 


let p_coef_ineq_of_p_coef_terms (t1, bop, t2 : p_coef term * bop * p_coef term): p_coef ineq =
  match bop with
  | LEQ -> add_p_coef_term t1 (uminus_p_coef_term t2)
  | GEQ -> add_p_coef_term (uminus_p_coef_term t1) t2
let p_coef_term_of_p_coef_ineq x = x

let p_coef_scalar_of_p_coef_ineq ineq =
  match List.filter is_p_coef_scalar (p_coef_v_list_of_p_coef_term @@ p_coef_term_of_p_coef_ineq ineq) with
  | [] -> zero_p_coef
  | hd :: _ -> uminus_p_coef @@ fst hd

let wo_p_coef_scalar_of_p_coef_ineq v_list ineq =
  let term = normalize_p_coef_term ineq in
  List.map
    (fun v ->
      try
        fst @@ List.find (fun (_, v') -> v' = v) (p_coef_v_list_of_p_coef_term term)
      with Not_found -> zero_p_coef)
    v_list



(* polytope *)
type 'coef polytope = 'coef ineq list

let unit_polytope = []
let add_ineq_to_polytope ineq polytope = ineq::polytope
let polytope_of_ineq_list l = l
let polytope_of_polytope_list list =  List.flatten list
let ineq_list_of_polytope x = x

let substitute_v_with_v_in_polytope (polytope: 'coef polytope) (v: v) (v':v): 'coef polytope = List.map (fun ineq -> substitute_v_with_v_in_ineq ineq v v') polytope

let v_list_of_polytope (polytope: 'coef polytope): v list =
  List.flatten @@ List.map (fun ineq -> v_list_of_ineq ineq) polytope

let string_of_c_coef_polytope (polytope: c_coef polytope) : string =
  "(polytope: " ^ string_of_list string_of_c_coef_ineq polytope ^ ")"
let string_of_c_coef_polytope' (polytope: c_coef polytope) : string =
  String.concat " /\\ " @@ List.map string_of_c_coef_ineq' polytope
let true_c_coef_polytope : c_coef polytope = []
let false_c_coef_polytope : c_coef polytope = [false_c_coef_ineq]
let unit_c_coef_polytope = true_c_coef_polytope
let ineq_list_of_uminus_c_coef_polytope = List.map uminus_c_coef_ineq
(* let normalize_coef_polytope = remove_duplicates *)
(* let c_coef_polytope_of_c_coef_terms (expr: c_coef term * bop * c_coef term) : c_coef polytope =
 *   [c_coef_ineq_of_c_coef_terms expr] *)

let string_of_p_coef_polytope (polytope: p_coef polytope) : string =
  "(polytope: " ^ string_of_list string_of_p_coef_ineq polytope ^ ")"
let true_p_coef_polytope : p_coef polytope = []
let unit_p_coef_polytope = true_p_coef_polytope
let false_p_coef_polytope : p_coef polytope = [false_p_coef_ineq]
let ineq_list_of_uminus_p_coef_polytope = List.map uminus_p_coef_ineq

(* 
let p_coef_polytope_of_p_coef_ineq_list = normalize_coef_polytope
let p_coef_polytope_of_p_coef_polytope_list list = normalize_coef_polytope @@ List.flatten list
 *)
let p_coef_ineq_list_of_p_coef_polytope x = x



(* linear predicate *)
type 'coef linpred = 'coef polytope list

let unit_linpred = []
let add_polytope_to_linpred polytope linpred = polytope::linpred 
let linpred_of_polytope_list = remove_duplicates
let linpred_of_linpred_list list = remove_duplicates @@ List.flatten list
let polytope_list_of_linpred x = x

let v_list_of_linpred (linpred: 'coef linpred): v list =
  List.flatten @@ List.map (fun polytope -> v_list_of_polytope polytope) linpred


let string_of_c_coef_linpred (linpred: c_coef linpred) : string =
  "(linpred: " ^ string_of_list (fun polytope -> "(polytope: " ^ string_of_list string_of_c_coef_ineq polytope ^ ")") linpred ^ ")"
let string_of_c_coef_linpred' (linpred: c_coef linpred) : string =
  String.concat " \\/ " @@ List.map string_of_c_coef_polytope' linpred
let true_c_coef_linpred : c_coef linpred = [true_c_coef_polytope]
let false_c_coef_linpred : c_coef linpred = []
let unit_c_coef_linpred = false_c_coef_linpred
let list_of_uminus_c_coef_linpred = List.map ineq_list_of_uminus_c_coef_polytope
(* let normalize_coef_linpred = List.map remove_duplicates *)

