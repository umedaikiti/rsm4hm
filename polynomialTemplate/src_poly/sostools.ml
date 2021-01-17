open Util

open Variable
open Coefficient
open Polynomial

open Pcfg
open Semialg
open Sequent
open Polynomial_template


type sostools = string

type positivstellensatz = Sch | Put

type gamma = Eq of float | Geq of float

let v_gamma = v_of_string_temporary "gamma"





let p_dummy = "p_dummy"

(** data conversion functions *)
let sostools_of_v = string_of_v
let sostools_of_param = string_of_param
let sostools_of_float : float -> sostools =
  fun f -> "(" ^ string_of_float f ^ "0" ^ ")"
let sostools_of_int = string_of_int
let sostools_of_list = string_of_list
let sostools_of_list_infix_string = string_of_list_infix_string

let sostools_of_c_coef : c_coef -> sostools =
  fun c_coef -> sostools_of_float @@ float_of_c_coef c_coef

let sostools_of_p_coef : p_coef -> sostools =
  fun p_coef ->
    let sostools_of_float_param (f, p) =
      if p = empty_param then sostools_of_float f
      (* 0.*p, 1.*p would be cumbersome. so we tranform them to 0., p. *)
      else if f = 0. then sostools_of_float 0.
      else if f = 1. then sostools_of_param p
      else sostools_of_float f ^ "*" ^ sostools_of_param p
    in
    let float_param_list = float_param_list_of_p_coef p_coef in
    (* 0. + p would be cumbersome. so we tranform it to p. *)
    let sostools_candidate_list =
      List.filter (fun s -> not (s = sostools_of_float 0.)) @@ List.map sostools_of_float_param float_param_list
    in
    if sostools_candidate_list = [] then sostools_of_float 0.
    else "(" ^ sostools_of_list_infix_string (fun x -> x) sostools_candidate_list "+" ^ ")"

let sostools_of_monomial : monomial -> sostools =
  fun monomial ->
    (if monomial = empty_monomial then sostools_of_float 1.
     else 
       let sostools_of_v_int (v, i) =
         if v = empty_v then sostools_of_float 1.
         else if i = 0 then sostools_of_float 1.
         else if i = 1 then sostools_of_v v
         else sostools_of_v v ^ "^" ^ sostools_of_int i
       in
       let v_int_list = v_int_list_of_monomial monomial in
       (* 1*x^i would be cumbersome. so we tranform it to x^i *)
       let sostools_candidate_list =
         List.filter (fun s -> not (s = sostools_of_float 1.)) @@ List.map sostools_of_v_int v_int_list
       in
       if sostools_candidate_list = [] then sostools_of_float 1.
       else "(" ^ sostools_of_list_infix_string (fun x -> x) sostools_candidate_list "*" ^ ")"
    )

let sostools_of_c_coef_polynomial : c_coef polynomial -> sostools =
  fun c_coef_polynomial ->
    let sostools_of_c_coef_monomial (c, m) =
      if m = empty_monomial then sostools_of_c_coef c
      else if c = zero_c_coef then sostools_of_c_coef zero_c_coef
      else if c = unit_c_coef then sostools_of_monomial m
      else sostools_of_c_coef c ^ "*" ^ sostools_of_monomial m
    in
    (* 0. + a*x^i would be cumbersome. so we tranform it to a*x^i *)
    let c_coef_monomial_list = c_coef_monomial_list_of_c_coef_polynomial c_coef_polynomial in
    let sostools_candidate_list =
      List.filter (fun s -> not (s = sostools_of_float 0.)) @@ List.map sostools_of_c_coef_monomial c_coef_monomial_list
    in
    if sostools_candidate_list = [] then sostools_of_float 0.
    else  "(" ^ sostools_of_list_infix_string sostools_of_c_coef_monomial c_coef_monomial_list "+" ^ ")"
    
let sostools_of_p_coef_polynomial : p_coef polynomial -> sostools =
  fun p_coef_polynomial ->
    let sostools_of_p_coef_monomial (p, m) =
      if m = empty_monomial then sostools_of_p_coef p
      else if p = zero_p_coef then sostools_of_p_coef zero_p_coef
      else if p = unit_p_coef then sostools_of_monomial m
      else sostools_of_p_coef p ^ "*" ^ sostools_of_monomial m
    in
    (* 0. + a*x^i would be cumbersome. so we tranform it to a*x^i *)
    let p_coef_monomial_list = p_coef_monomial_list_of_p_coef_polynomial p_coef_polynomial in
    let sostools_candidate_list =
      List.filter (fun s -> not (s = sostools_of_float 0.)) @@ List.map sostools_of_p_coef_monomial p_coef_monomial_list
    in
    if sostools_candidate_list = [] then sostools_of_float 0.
    else  "(" ^ sostools_of_list_infix_string sostools_of_p_coef_monomial p_coef_monomial_list "+" ^ ")"


(** ===================
    sostools statements *)

let sostools_of_objective_function : p_coef -> sostools =
  fun obj ->
    sostools_of_p_coef obj
    
let sostools_syms_list_of_param_domain : param_domain -> sostools =
  fun template_variables ->
    let param_list = param_list_of_param_domain template_variables in
    sostools_of_list_infix_string sostools_of_param param_list " "

let sostools_of_variable_declaration : param_domain -> string -> sostools =
  fun template_variables sdp_program_name ->
    let param_list = param_list_of_param_domain template_variables in
    let str_var_list = sostools_of_list sostools_of_param param_list in
    sdp_program_name ^ " = sosdecvar(" ^  sdp_program_name ^ ", " ^  str_var_list ^ ");"

let constraint_sostools l sdp_program_name (h_w_name_w_list_rev : (string * int) list) antecedents consequent sostools_opt x_dummy_option new_decvartable : sostools =
  let buf = Buffer.create (1024 * 1024) in

  let sostools_of_vec_gw gi_list w =
    let sostools_of_gi = sostools_of_c_coef_polynomial in
    let rec loop ret gi_list w' =
      match (gi_list, w') with
      | ([], w') when w' = 0 -> ret
      | (gi::gi_list', w') when w' land 1 = 0 -> loop ret gi_list' (w' lsr 1)
      | (gi::gi_list', w') when w' land 1 = 1 -> loop (sostools_of_gi gi::ret) gi_list' (w' lsr 1)
      | _ -> failwith "constraint_sostools: sostools_of_vec_gw"
    in
    let sostools_vec_gw = loop [] (List.rev gi_list) w in
    sostools_of_list_infix_string (fun x -> x) ~string_of_empty_list:"1" sostools_vec_gw "*"
  in

  let sostools_of_hw_vec_g_w l gi_list hw w =
    let sostools_vec_gw = sostools_of_vec_gw gi_list w in
    hw ^ "*" ^ sostools_vec_gw
  in
  let rec loop = function
  | [] -> Buffer.add_string buf "0.0"
  | [(hw,w)] -> Buffer.add_string buf (sostools_of_hw_vec_g_w l antecedents hw w)
  | (hw,w)::rest ->
     Buffer.add_string buf (sostools_of_hw_vec_g_w l antecedents hw w);
     Buffer.add_string buf "+";
     loop rest in
  Buffer.add_string buf ("% " ^ (if antecedents = [] then "true" else String.concat "/\\" @@ List.map sostools_of_c_coef_polynomial antecedents) ^ " -> " ^ sostools_of_p_coef_polynomial consequent ^ "\n");
  Buffer.add_string buf (sdp_program_name ^ " = "); 
  Buffer.add_string buf (if sostools_opt then "my_sosineq" else "sosineq");
  Buffer.add_string buf ("(" ^  sdp_program_name ^ ", "); 
  Buffer.add_string buf "-(";
  loop h_w_name_w_list_rev;
  Buffer.add_string buf ") + ";
  Buffer.add_string buf (sostools_of_p_coef_polynomial consequent);
  (match x_dummy_option with
   | Some x_dummy -> Buffer.add_string buf ("+ 0.0 * " ^ string_of_v x_dummy)
   | None -> ());
  Buffer.add_string buf new_decvartable; 
  Buffer.add_string buf (", 'sparse');");
  Buffer.contents buf


let sostools_of_sequent sdp_program_name sostools_monomial_vector ((antecedents, consequent), (seq_idx:int)) positivstellensatz sostools_opt x_dummy : sostools list * (string -> sostools) * param list =
  let m = List.length antecedents in

  let h_w_name_of_idx_vector seq_idx  idx_vector =
    "h_" ^
    string_of_int_hexadecimal idx_vector ^
    "_seqno" ^
    sostools_of_int seq_idx in

  let h_w_name_w_list_rev =
    match positivstellensatz with
    | Sch ->
       let rec get_h_w_name_list_rev ret n =
         if n <= 0 then ret (*(h_w_name_of_idx_vector seq_idx 0, 0)::ret *)
         else get_h_w_name_list_rev ((h_w_name_of_idx_vector seq_idx n, n)::ret) (n-1) in
       get_h_w_name_list_rev [] (pow2 m - 1) 
    | Put ->
       let rec get_h_i_name_list_rev ret n =
         if n = 0 then ret (*(h_w_name_of_idx_vector seq_idx 0, 0)::ret *)
         else get_h_i_name_list_rev ((h_w_name_of_idx_vector seq_idx n, n)::ret) (n lsr 1) in
       get_h_i_name_list_rev [] (pow2 (m - 1)) 
  in

  let sostools_of_sos_var_declaration sdp_program_name h_w_name : sostools =
    "[" ^ sdp_program_name ^ ", " ^  h_w_name ^ "]"
    ^ " = sossosvar(" ^  sdp_program_name ^ ", " ^ sostools_monomial_vector ^ ");"
  in

  let sos_declarations =
    List.rev_map (fun (h_w_name, _) -> sostools_of_sos_var_declaration sdp_program_name h_w_name) h_w_name_w_list_rev
  in

  let param_list = collect_param_string_list_of_p_coef_polynomial consequent in
  let x_dummy_option = if sos_declarations = [] && param_list = [] then Some x_dummy else None in

  (List.rev sos_declarations, constraint_sostools seq_idx sdp_program_name h_w_name_w_list_rev antecedents consequent sostools_opt x_dummy_option, param_list)



let sostools_list_of_sdp_constraint : (and_ineq_list * p_coef ineq) list -> string -> string -> positivstellensatz -> bool -> v -> sostools list =
  fun sdp_constraint sostools_monomial_vector sdp_program_name  positivstellensatz sostools_opt x_dummy ->
  List.rev @@
  List.fold_left
    (fun ret x ->
      let (dec_list_new, constr, param_list) = sostools_of_sequent sdp_program_name sostools_monomial_vector x positivstellensatz sostools_opt x_dummy in
      if not sostools_opt then ""::(constr "")::(List.rev_append dec_list_new ret) else
      if dec_list_new = [] && param_list = [] then ""::(constr ", []")::ret else
      if dec_list_new = [] then ""::(constr (", [" ^ sostools_of_list_infix_string sostools_of_param param_list "; " ^ "]"))::List.rev_append dec_list_new ret else
      let sostools_number_of_coeffs =  sdp_program_name ^ ".var.idx{"^ sdp_program_name ^ ".var.num+1} - " ^ sdp_program_name ^ ".var.idx{1}" in
      let f string_of_a ?string_of_empty_list:(string_of_empty_list="") list s =
        if list = [] then string_of_empty_list else
          let buf = Buffer.create ((String.length (string_of_a (List.hd list)) + 5) * List.length list) in
          let rec string_of_list_infix_string_sub list =
            match list with
            | [] -> ()
            | hd :: tl -> Buffer.add_string buf (string_of_a hd ^ s); string_of_list_infix_string_sub tl in
          string_of_list_infix_string_sub list;
          Buffer.contents buf in
      let sostools_get_newdecvartable = [
          "new_decvartable = [" ^ f sostools_of_param param_list "; " ^ "(arrayfun(@(x) ['coeff_', int2str(x)], [1+old_coeffs:(" ^ sdp_program_name ^ ".var.idx{" ^  sdp_program_name ^ ".var.num+1} - " ^ sdp_program_name ^ ".var.idx{1})],'UniformOutput',false))'];"
        ] in
      ""::(constr ", new_decvartable")::List.rev_append sostools_get_newdecvartable (List.rev_append dec_list_new (("old_coeffs = " ^ sostools_number_of_coeffs ^ ";")::ret)) 
    )
    []
    (List.combine
       (List.map
          (fun (and_ineq_list, p_coef_ineq) ->
            (List.map c_coef_polynomial_of_c_coef_ineq @@ c_coef_ineq_list_of_and_ineq_list and_ineq_list,
             p_coef_polynomial_of_p_coef_ineq p_coef_ineq))
          sdp_constraint)
       (1 -- List.length sdp_constraint))

(*
(** data conversion functions *)
let sostools_of_v = string_of_v
let sostools_of_param = string_of_param
let sostools_of_float : float -> sostools =
  fun f -> "(" ^ string_of_float f ^ "0" ^ ")"
let sostools_of_int = string_of_int
let sostools_of_list = string_of_list
let sostools_of_list_infix_string = string_of_list_infix_string

let sostools_of_c_coef : c_coef -> sostools =
  fun c_coef -> sostools_of_float @@ float_of_c_coef c_coef

let sostools_of_p_coef : p_coef -> sostools =
  fun p_coef ->
    let sostools_of_float_param (f, p) =
      if p = empty_param then sostools_of_float f
      (* 0.*p, 1.*p would be cumbersome. so we tranform them to 0., p. *)
      else if f = 0. then sostools_of_float 0.
      else if f = 1. then sostools_of_param p
      else sostools_of_float f ^ "*" ^ sostools_of_param p
    in
    let float_param_list = float_param_list_of_p_coef p_coef in
    (* 0. + p would be cumbersome. so we tranform it to p. *)
    let sostools_candidate_list =
      List.filter (fun s -> not (s = sostools_of_float 0.)) @@ List.map sostools_of_float_param float_param_list
    in
    if sostools_candidate_list = [] then sostools_of_float 0.
    else "(" ^ sostools_of_list_infix_string (fun x -> x) sostools_candidate_list "+" ^ ")"

let sostools_of_monomial : monomial -> sostools =
  fun monomial ->
    (if monomial = empty_monomial then sostools_of_float 1.
     else 
       let sostools_of_v_int (v, i) =
         if v = empty_v then sostools_of_float 1.
         else if i = 0 then sostools_of_float 1.
         else if i = 1 then sostools_of_v v
         else sostools_of_v v ^ "^" ^ sostools_of_int i
       in
       let v_int_list = v_int_list_of_monomial monomial in
       (* 1*x^i would be cumbersome. so we tranform it to x^i *)
       let sostools_candidate_list =
         List.filter (fun s -> not (s = sostools_of_float 1.)) @@ List.map sostools_of_v_int v_int_list
       in
       if sostools_candidate_list = [] then sostools_of_float 1.
       else "(" ^ sostools_of_list_infix_string (fun x -> x) sostools_candidate_list "*" ^ ")"
    )

let sostools_of_c_coef_polynomial : c_coef polynomial -> sostools =
  fun c_coef_polynomial ->
    let sostools_of_c_coef_monomial (c, m) =
      if m = empty_monomial then sostools_of_c_coef c
      else if c = zero_c_coef then sostools_of_c_coef zero_c_coef
      else if c = unit_c_coef then sostools_of_monomial m
      else sostools_of_c_coef c ^ "*" ^ sostools_of_monomial m
    in
    (* 0. + a*x^i would be cumbersome. so we tranform it to a*x^i *)
    let c_coef_monomial_list = c_coef_monomial_list_of_c_coef_polynomial c_coef_polynomial in
    let sostools_candidate_list =
      List.filter (fun s -> not (s = sostools_of_float 0.)) @@ List.map sostools_of_c_coef_monomial c_coef_monomial_list
    in
    if sostools_candidate_list = [] then sostools_of_float 0.
    else  "(" ^ sostools_of_list_infix_string sostools_of_c_coef_monomial c_coef_monomial_list "+" ^ ")"
    
let sostools_of_p_coef_polynomial : p_coef polynomial -> sostools =
  fun p_coef_polynomial ->
    let sostools_of_p_coef_monomial (p, m) =
      if m = empty_monomial then sostools_of_p_coef p
      else if p = zero_p_coef then sostools_of_p_coef zero_p_coef
      else if p = unit_p_coef then sostools_of_monomial m
      else sostools_of_p_coef p ^ "*" ^ sostools_of_monomial m
    in
    (* 0. + a*x^i would be cumbersome. so we tranform it to a*x^i *)
    let p_coef_monomial_list = p_coef_monomial_list_of_p_coef_polynomial p_coef_polynomial in
    let sostools_candidate_list =
      List.filter (fun s -> not (s = sostools_of_float 0.)) @@ List.map sostools_of_p_coef_monomial p_coef_monomial_list
    in
    if sostools_candidate_list = [] then sostools_of_float 0.
    else  "(" ^ sostools_of_list_infix_string sostools_of_p_coef_monomial p_coef_monomial_list "+" ^ ")"


(** ===================
    sostools statements *)

let sostools_of_objective_function : p_coef -> sostools =
  fun obj ->
    sostools_of_p_coef obj
    
let sostools_syms_list_of_param_domain : param_domain -> sostools =
  fun template_variables ->
    let param_list = param_list_of_param_domain template_variables in
    sostools_of_list_infix_string sostools_of_param param_list " "

let sostools_of_variable_declaration : param_domain -> string -> sostools =
  fun template_variables sdp_program_name ->
    let param_list = param_list_of_param_domain template_variables in
    let str_var_list = sostools_of_list sostools_of_param param_list in
    sdp_program_name ^ " = sosdecvar(" ^  sdp_program_name ^ ", " ^  str_var_list ^ ");"

let constraint_sostools h_w_name_of_idx_vector l sdp_program_name antecedents consequent : sostools =
  let m = List.length antecedents in

  let buf = Buffer.create (1024 * 1024) in
  Buffer.add_string buf (sdp_program_name ^ " = soseq(" ^  sdp_program_name ^ ", "); 
  let sostools_of_gi_list l antecedents =

    let sostools_of_vec_gw gi_list w =
      let sostools_of_gi = sostools_of_c_coef_polynomial in
      let rec loop ret gi_list w' =
        match (gi_list, w') with
        | ([], w') when w' = 0 -> ret
        | (gi::gi_list', w') when w' land 1 = 0 -> loop ret gi_list' (w' lsr 1)
        | (gi::gi_list', w') when w' land 1 = 1 -> loop (sostools_of_gi gi::ret) gi_list' (w' lsr 1)
        | _ -> failwith "constraint_sostools: sostools_of_vec_gw"
      in
      let sostools_vec_gw = loop [] (List.rev gi_list) w in
      sostools_of_list_infix_string (fun x -> x) ~string_of_empty_list:"1" sostools_vec_gw "*"
    in

    let sostools_of_hw_vec_g_w l gi_list w =
      let hw = h_w_name_of_idx_vector l w in
      let sostools_vec_gw = sostools_of_vec_gw gi_list w in
      hw ^ "*" ^ sostools_vec_gw
    in
    let rec loop n =
      if n >= pow2 m then ()
      else ((if n <= 0 then
               Buffer.add_string buf (sostools_of_hw_vec_g_w l antecedents n)
             else
               (Buffer.add_string buf "+";
               Buffer.add_string buf (sostools_of_hw_vec_g_w l antecedents n)));
            loop (n+1)) in
    loop 0
  in
  sostools_of_gi_list l antecedents;
  Buffer.add_string buf " - ";
  Buffer.add_string buf (sostools_of_p_coef_polynomial consequent);
  Buffer.add_string buf ");";
  Buffer.contents buf


let sostools_of_sequent sdp_program_name deg v_domain ((antecedents, consequent), (seq_idx:int)) : sostools list * sostools =
  let m = List.length antecedents in

  let h_w_name_of_idx_vector seq_idx idx_vector =
    "h_" ^
    string_of_int_hexadecimal idx_vector ^
    "_seqno" ^
    sostools_of_int seq_idx in

  let monomial_vector = possible_monomial_list v_domain deg in
  let h_w_name_list_rev =
    let rec get_h_w_name_list_rev ret n =
      if n < 0 then ret
      else get_h_w_name_list_rev (h_w_name_of_idx_vector seq_idx n::ret) (n-1) in
    get_h_w_name_list_rev [] (pow2 m - 1) in

  let sostools_of_sos_var_declaration sdp_program_name h_w_name monomial_vector : sostools =
    "[" ^ sdp_program_name ^ ", " ^  h_w_name ^ "]"
    ^ " = sossosvar(" ^  sdp_program_name ^ ", " ^  sostools_of_list sostools_of_monomial monomial_vector ^ ");"
  in

  let sos_declarations =
    List.rev_map (fun h_w_name -> sostools_of_sos_var_declaration sdp_program_name h_w_name monomial_vector) h_w_name_list_rev
  in
  (sos_declarations, constraint_sostools h_w_name_of_idx_vector seq_idx sdp_program_name antecedents consequent)


let sostools_list_of_sdp_constraint : (and_ineq_list * p_coef ineq) list -> v_domain -> int -> string -> sostools list =
  fun sdp_constraint v_domain deg sdp_program_name ->
  let dec_list_rev, constraint_list =
    List.fold_left
      (fun (dec_list, constraint_list) x ->
        let (dec_list_new, constr) = sostools_of_sequent sdp_program_name deg v_domain x in
        (List.rev_append dec_list_new dec_list, constr::constraint_list))
      ([],[])
      (List.combine
         (List.map
            (fun (and_ineq_list, p_coef_ineq) ->
              (List.map c_coef_polynomial_of_c_coef_ineq @@ c_coef_ineq_list_of_and_ineq_list and_ineq_list,
               p_coef_polynomial_of_p_coef_ineq p_coef_ineq))
            sdp_constraint)
         (1 -- List.length sdp_constraint)) in
  List.rev_append dec_list_rev (List.rev constraint_list)
*)
(*
let is_false_c_coef_ineq (p: c_coef polynomial) : bool =
  match p with
  | [(c, empty_monomial)] -> float_of_c_coef c < 0.
  | _ -> false
*)
let rec simplify_constraint : (and_ineq_list * p_coef ineq) list -> (and_ineq_list * p_coef ineq) list = function
  [] -> []
  | (antecedents, consequent)::c' -> if List.mem false_c_coef_ineq @@ c_coef_ineq_list_of_and_ineq_list antecedents then simplify_constraint c' else (and_ineq_list_of_c_coef_ineq_list @@ List.filter (fun a -> a <> true_c_coef_ineq) @@ c_coef_ineq_list_of_and_ineq_list antecedents, consequent)::simplify_constraint c'

let rec list_init' i len f = if i < len then f i :: list_init' (i+1) len f else []
let list_init len f = if len < 0 then raise (Invalid_argument "len < 0") else list_init' 0 len f

let sostools_of_reachability_problem (((pcfg_transition, l_domain, v_domain), (l_init,init_valuation), invariant, term_config, rewardmap, order) as problem) (template_deg: int) (sos_deg: int) (sdp_program_name: string) : sostools =
  let template_list, template_param_domain_list = List.split @@ (*List.init*) list_init order (fun i -> generate_polynomial_template l_domain v_domain template_deg ("o" ^ string_of_int (i+1))) in
  let sequents, objective_func, new_v_list = sequents_and_objective_func_of_reachability_problem problem template_list in


  let (v_list, x_dummy) =
    let v_list = new_v_list @ (v_list_of_v_domain v_domain) in
    if v_list = [] then ([v_of_string_temporary "x_dummy"], v_of_string_temporary "x_dummy")
    else (v_list, List.hd v_list) in
  let param_list = List.flatten @@ List.map param_list_of_param_domain template_param_domain_list in

  let sostools_syms_list = 
    sostools_of_list_infix_string sostools_of_v v_list " " ^ " " ^
      sostools_of_list_infix_string sostools_of_param param_list " " ^ (if param_list = [] then " " ^ p_dummy else "")
  in

  let buf = Buffer.create (1024 * 1024) in
  buffer_add_line buf "clear;";
  buffer_add_line buf "";


  let sostools_declaration_v_domain = "syms " ^ sostools_syms_list ^ ";" in
  buffer_add_line buf sostools_declaration_v_domain; 
  buffer_add_line buf "";


  let sostools_dec_arg =
    "[" ^ sostools_of_list_infix_string sostools_of_v v_list "; " ^ "], " ^
      "[" ^ (if param_list = [] then "p_dummy" else sostools_of_list_infix_string sostools_of_param param_list "; ") ^ "]"
  in
  let sostools_initialization = 
    sdp_program_name ^ " = sosprogram(" ^ sostools_dec_arg ^ ");"
  in
  buffer_add_line buf sostools_initialization;
  buffer_add_line buf "";

  (* a vector of monomials *)
  let sostools_monomial_vector = sdp_program_name ^ "_monomial_vector" in
  buffer_add_line buf "% the vector of monomials";
  buffer_add_line buf (sostools_monomial_vector ^ " = monomials([" ^ sostools_of_list_infix_string sostools_of_v v_list "; " ^ "], [" ^ sostools_of_list_infix_string string_of_int (0 -- sos_deg) " " ^ "]);");
  buffer_add_line buf "";


  (* sostools_of_variable_declaration template_variables sdp_program_name :: *)
  List.iter
    (fun (s,sequent) ->
      buffer_add_line buf ("% "^ s);
      List.iter (buffer_add_line buf) (sostools_list_of_sdp_constraint sequent sostools_monomial_vector sdp_program_name Sch true x_dummy);
      buffer_add_line buf "")
    sequents;

  let str_obj = sostools_of_objective_function objective_func in
  let sostools_set_objective_function =
    sdp_program_name ^ " = sossetobj(" ^  sdp_program_name ^ ", obj);" in
  buffer_add_line buf "% setting options";
  buffer_add_line buf "option.solver = 'sdpt3';";
(*  buffer_add_line buf "option.solver = 'sedumi';";*)
  buffer_add_line buf "option.params.maxit = 130;";
  buffer_add_line buf "% setting objective function";
  buffer_add_line buf ("obj = " ^ str_obj ^ ";");
  buffer_add_line buf sostools_set_objective_function;
  buffer_add_line buf "";

  let sostools_call_solver = sdp_program_name ^ " = sossolve(" ^ sdp_program_name ^ ", option)" in
  buffer_add_line buf "% calling solver";
  buffer_add_line buf sostools_call_solver;
  buffer_add_line buf "";

  let sostools_solve = "sol = sosgetsol(" ^ sdp_program_name ^ ", obj)" in
  buffer_add_line buf "% getting a bound";
  buffer_add_line buf sostools_solve;

  Buffer.contents buf

(*
  let v_list = new_v_list @ (v_list_of_v_domain v_domain) in
  let param_list = List.flatten @@ List.map param_list_of_param_domain template_param_domain_list in
  let sostools_syms_list =
    sostools_of_list_infix_string sostools_of_v v_list " " ^ " " ^
      sostools_of_list_infix_string sostools_of_param param_list " "
  in

  let buf = Buffer.create (1024 * 1024) in
  buffer_add_line buf "clear;";
  buffer_add_line buf "";


  let sostools_declaration_v_domain = "syms " ^ sostools_syms_list ^ ";" in
  buffer_add_line buf sostools_declaration_v_domain; 
  buffer_add_line buf "";


  let sostools_dec_arg =
    "[" ^ sostools_of_list_infix_string sostools_of_v v_list "; " ^ "], " ^
      "[" ^ sostools_of_list_infix_string sostools_of_param param_list "; " ^ "]"
  in
  let sostools_initialization = 
    sdp_program_name ^ " = sosprogram(" ^ sostools_dec_arg ^ ");"
  in
  buffer_add_line buf sostools_initialization;
  buffer_add_line buf "";


  (* sostools_of_variable_declaration template_variables sdp_program_name :: *)
  List.iter
    (fun (s,sequent) ->
      buffer_add_line buf ("% "^ s);
      List.iter (buffer_add_line buf) (sostools_list_of_sdp_constraint sequent v_domain sos_deg sdp_program_name);
      buffer_add_line buf "")
    sequents;

  let str_obj = sostools_of_objective_function objective_func in
  let sostools_set_objective_function =
    sdp_program_name ^ " = sossetobj(" ^  sdp_program_name ^ ", obj);" in
  buffer_add_line buf "% setting options";
  buffer_add_line buf "option.solver = 'sdpt3';";
  buffer_add_line buf "option.params.maxit = 130;";
  buffer_add_line buf "% setting objective function";
  buffer_add_line buf ("obj = " ^ str_obj ^ ";");
  buffer_add_line buf sostools_set_objective_function;
  buffer_add_line buf "";

  let sostools_call_solver = sdp_program_name ^ " = sossolve(" ^ sdp_program_name ^ ", option)" in
  buffer_add_line buf "% calling solver";
  buffer_add_line buf sostools_call_solver;
  buffer_add_line buf "";

  let sostools_solve = "sol = sosgetsol(" ^ sdp_program_name ^ ", obj)" in
  buffer_add_line buf "% getting a bound";
  buffer_add_line buf sostools_solve;

  Buffer.contents buf
*)
