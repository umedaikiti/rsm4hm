open Util

open Variable
open Coefficient
open Polynomial

open Pcfg
open Semialg
open Sequent
open Polynomial_template


type julia_sos = string

(** data conversion functions *)
let julia_sos_of_v = string_of_v
let julia_sos_of_param = string_of_param
let julia_sos_of_float : float -> julia_sos =
  fun f -> "(" ^ string_of_float f ^ "0" ^ ")"
let julia_sos_of_int = string_of_int
let julia_sos_of_list = string_of_list
let julia_sos_of_list_infix_string = string_of_list_infix_string

let julia_sos_of_c_coef : c_coef -> julia_sos =
  fun c_coef -> julia_sos_of_float @@ float_of_c_coef c_coef

let julia_sos_of_p_coef : p_coef -> julia_sos =
  fun p_coef ->
    let julia_sos_of_float_param (f, p) =
      if p = empty_param then julia_sos_of_float f
      (* 0.*p, 1.*p would be cumbersome. so we tranform them to 0., p. *)
      else if f = 0. then julia_sos_of_float 0.
      else if f = 1. then julia_sos_of_param p
      else julia_sos_of_float f ^ "*" ^ julia_sos_of_param p
    in
    let float_param_list = float_param_list_of_p_coef p_coef in
    (* 0. + p would be cumbersome. so we tranform it to p. *)
    let julia_sos_candidate_list =
      List.filter (fun s -> not (s = julia_sos_of_float 0.)) @@ List.map julia_sos_of_float_param float_param_list
    in
    if julia_sos_candidate_list = [] then julia_sos_of_float 0.
    else "(" ^ julia_sos_of_list_infix_string (fun x -> x) julia_sos_candidate_list "+" ^ ")"

let julia_sos_of_monomial : monomial -> julia_sos =
  fun monomial ->
    (if monomial = empty_monomial then julia_sos_of_float 1.
     else 
       let julia_sos_of_v_int (v, i) =
         if v = empty_v then julia_sos_of_float 1.
         else if i = 0 then julia_sos_of_float 1.
         else if i = 1 then julia_sos_of_v v
         else julia_sos_of_v v ^ "^" ^ julia_sos_of_int i
       in
       let v_int_list = v_int_list_of_monomial monomial in
       (* 1*x^i would be cumbersome. so we tranform it to x^i *)
       let julia_sos_candidate_list =
         List.filter (fun s -> not (s = julia_sos_of_float 1.)) @@ List.map julia_sos_of_v_int v_int_list
       in
       if julia_sos_candidate_list = [] then julia_sos_of_float 1.
       else "(" ^ julia_sos_of_list_infix_string (fun x -> x) julia_sos_candidate_list "*" ^ ")"
    )

let julia_sos_of_c_coef_polynomial : c_coef polynomial -> julia_sos =
  fun c_coef_polynomial ->
    let julia_sos_of_c_coef_monomial (c, m) =
      if m = empty_monomial then julia_sos_of_c_coef c
      else if c = zero_c_coef then julia_sos_of_c_coef zero_c_coef
      else if c = unit_c_coef then julia_sos_of_monomial m
      else julia_sos_of_c_coef c ^ "*" ^ julia_sos_of_monomial m
    in
    (* 0. + a*x^i would be cumbersome. so we tranform it to a*x^i *)
    let c_coef_monomial_list = c_coef_monomial_list_of_c_coef_polynomial c_coef_polynomial in
    let julia_sos_candidate_list =
      List.filter (fun s -> not (s = julia_sos_of_float 0.)) @@ List.map julia_sos_of_c_coef_monomial c_coef_monomial_list
    in
    if julia_sos_candidate_list = [] then julia_sos_of_float 0.
    else  "(" ^ julia_sos_of_list_infix_string julia_sos_of_c_coef_monomial c_coef_monomial_list "+" ^ ")"
    
let julia_sos_of_p_coef_polynomial : p_coef polynomial -> julia_sos =
  fun p_coef_polynomial ->
    let julia_sos_of_p_coef_monomial (p, m) =
      if m = empty_monomial then julia_sos_of_p_coef p
      else if p = zero_p_coef then julia_sos_of_p_coef zero_p_coef
      else if p = unit_p_coef then julia_sos_of_monomial m
      else julia_sos_of_p_coef p ^ "*" ^ julia_sos_of_monomial m
    in
    (* 0. + a*x^i would be cumbersome. so we tranform it to a*x^i *)
    let p_coef_monomial_list = p_coef_monomial_list_of_p_coef_polynomial p_coef_polynomial in
    let julia_sos_candidate_list =
      List.filter (fun s -> not (s = julia_sos_of_float 0.)) @@ List.map julia_sos_of_p_coef_monomial p_coef_monomial_list
    in
    if julia_sos_candidate_list = [] then julia_sos_of_float 0.
    else  "(" ^ julia_sos_of_list_infix_string julia_sos_of_p_coef_monomial p_coef_monomial_list "+" ^ ")"

let julia_sos_of_p_coef_ineq p_coef_ineq = (julia_sos_of_p_coef_polynomial @@ p_coef_polynomial_of_p_coef_ineq p_coef_ineq) ^ " >= 0"

let julia_sos_of_c_coef_ineq c_coef_ineq = (julia_sos_of_c_coef_polynomial @@ c_coef_polynomial_of_c_coef_ineq c_coef_ineq) ^ " >= 0"

let julia_sos_of_and_ineq_list and_ineq_list : julia_sos = "@set " ^ String.concat " && " @@ List.map julia_sos_of_c_coef_ineq @@ c_coef_ineq_list_of_and_ineq_list and_ineq_list

(** ===================
    julia_sos statements *)

let julia_sos_of_objective_function : p_coef -> julia_sos =
  fun obj ->
    julia_sos_of_p_coef obj
    
let julia_sos_syms_list_of_param_domain : param_domain -> julia_sos =
  fun template_variables ->
    let param_list = param_list_of_param_domain template_variables in
    julia_sos_of_list_infix_string julia_sos_of_param param_list " "

let julia_sos_of_variable_declaration : param_domain -> string -> julia_sos =
  fun template_variables sdp_program_name ->
    let param_list = param_list_of_param_domain template_variables in
    let str_var_list = julia_sos_of_list julia_sos_of_param param_list in
    sdp_program_name ^ " = sosdecvar(" ^  sdp_program_name ^ ", " ^  str_var_list ^ ");"

let constraint_julia_sos h_w_name_of_idx_vector l sdp_program_name antecedents consequent : julia_sos =
  let m = List.length antecedents in

  let buf = Buffer.create (1024 * 1024) in
  Buffer.add_string buf (sdp_program_name ^ " = soseq(" ^  sdp_program_name ^ ", "); 
  let julia_sos_of_gi_list l antecedents =

    let julia_sos_of_vec_gw gi_list w =
      let julia_sos_of_gi = julia_sos_of_c_coef_polynomial in
      let rec loop ret gi_list w' =
        match (gi_list, w') with
        | ([], w') when w' = 0 -> ret
        | (gi::gi_list', w') when w' land 1 = 0 -> loop ret gi_list' (w' lsr 1)
        | (gi::gi_list', w') when w' land 1 = 1 -> loop (julia_sos_of_gi gi::ret) gi_list' (w' lsr 1)
        | _ -> failwith "constraint_julia_sos: julia_sos_of_vec_gw"
      in
      let julia_sos_vec_gw = loop [] (List.rev gi_list) w in
      julia_sos_of_list_infix_string (fun x -> x) ~string_of_empty_list:"1" julia_sos_vec_gw "*"
    in

    let julia_sos_of_hw_vec_g_w l gi_list w =
      let hw = h_w_name_of_idx_vector l w in
      let julia_sos_vec_gw = julia_sos_of_vec_gw gi_list w in
      hw ^ "*" ^ julia_sos_vec_gw
    in
    let rec loop n =
      if n >= pow2 m then ()
      else ((if n <= 0 then
               Buffer.add_string buf (julia_sos_of_hw_vec_g_w l antecedents n)
             else
               (Buffer.add_string buf "+";
               Buffer.add_string buf (julia_sos_of_hw_vec_g_w l antecedents n)));
            loop (n+1)) in
    loop 0
  in
  julia_sos_of_gi_list l antecedents;
  Buffer.add_string buf " - ";
  Buffer.add_string buf (julia_sos_of_p_coef_polynomial consequent);
  Buffer.add_string buf ");";
  Buffer.contents buf


let julia_sos_of_sequent sdp_program_name deg v_domain ((antecedents, consequent), (seq_idx:int)) : julia_sos list * julia_sos =
  let m = List.length antecedents in

  let h_w_name_of_idx_vector seq_idx idx_vector =
    "h_" ^
    string_of_int_hexadecimal idx_vector ^
    "_seqno" ^
    julia_sos_of_int seq_idx in

  let monomial_vector = possible_monomial_list v_domain deg in
  let h_w_name_list_rev =
    let rec get_h_w_name_list_rev ret n =
      if n < 0 then ret
      else get_h_w_name_list_rev (h_w_name_of_idx_vector seq_idx n::ret) (n-1) in
    get_h_w_name_list_rev [] (pow2 m - 1) in

  let julia_sos_of_sos_var_declaration sdp_program_name h_w_name monomial_vector : julia_sos =
    "[" ^ sdp_program_name ^ ", " ^  h_w_name ^ "]"
    ^ " = sossosvar(" ^  sdp_program_name ^ ", " ^  julia_sos_of_list julia_sos_of_monomial monomial_vector ^ ");"
  in

  let sos_declarations =
    List.rev_map (fun h_w_name -> julia_sos_of_sos_var_declaration sdp_program_name h_w_name monomial_vector) h_w_name_list_rev
  in
  (sos_declarations, constraint_julia_sos h_w_name_of_idx_vector seq_idx sdp_program_name antecedents consequent)


let julia_sos_list_of_sdp_constraint : (and_ineq_list * p_coef ineq) list -> v_domain -> int -> string -> julia_sos list =
  fun sdp_constraint v_domain deg sdp_program_name ->
  let dec_list_rev, constraint_list =
    List.fold_left
      (fun (dec_list, constraint_list) x ->
        let (dec_list_new, constr) = julia_sos_of_sequent sdp_program_name deg v_domain x in
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

let rec list_init' i len f = if i < len then f i :: list_init' (i+1) len f else []
let list_init len f = if len < 0 then raise (Invalid_argument "len < 0") else list_init' 0 len f

let julia_sos_of_reachability_problem (((pcfg_transition, l_domain, v_domain), (l_init,init_valuation), invariant, term_config, rewardmap, order) as problem) (template_deg: int) (sos_deg: int) (sdp_program_name: string) : julia_sos =
  let template_list, template_param_domain_list = List.split @@ (*List.init*) list_init order (fun i -> generate_polynomial_template l_domain v_domain template_deg ("o" ^ string_of_int (i+1))) in
  let sequents, objective_func, new_v_list = sequents_and_objective_func_of_reachability_problem problem template_list in

  let v_list = new_v_list @ (v_list_of_v_domain v_domain) in
  let param_list = List.flatten @@ List.map param_list_of_param_domain template_param_domain_list in

  let buf = Buffer.create (1024 * 1024) in
  buffer_add_line buf "using MultivariatePolynomials";
  buffer_add_line buf "using JuMP";
  buffer_add_line buf "using PolyJuMP";
  buffer_add_line buf "using SumOfSquares";
  buffer_add_line buf "using DynamicPolynomials";
  buffer_add_line buf "using CSDP";
  buffer_add_line buf "using SemialgebraicSets";
  buffer_add_line buf "";
  buffer_add_line buf "m = SOSModel(solver = CSDPSolver())";
  buffer_add_line buf "";

  let julia_sos_declaration_v_domain = "@polyvar " ^ julia_sos_of_list_infix_string julia_sos_of_v v_list " " in
  buffer_add_line buf julia_sos_declaration_v_domain; 
  buffer_add_line buf "";

  let julia_sos_declaration_param_list = String.concat "" @@ List.map (fun p -> Printf.sprintf "@variable m %s\n" @@ string_of_param p) param_list in
  buffer_add_line buf julia_sos_declaration_param_list; 

  let str_obj = julia_sos_of_objective_function objective_func in
  buffer_add_line buf "@variable m obj";
  buffer_add_line buf @@ Printf.sprintf "@constraint m %s == obj" str_obj;
  buffer_add_line buf "@objective m Min obj";
  buffer_add_line buf "";

  List.iter
    (fun (s,sequent) ->
      buffer_add_line buf ("# "^ s);
      (List.iter
        (fun (and_ineq_list, p_coef_ineq) ->
          let const_string = julia_sos_of_p_coef_ineq p_coef_ineq in
          let s = if and_ineq_list = true_and_ineq_list then Printf.sprintf "@constraint(m, %s)" const_string else Printf.sprintf "@constraint(m, %s, domain = %s)" const_string @@ julia_sos_of_and_ineq_list and_ineq_list in
          buffer_add_line buf s
        ) sequent);
      buffer_add_line buf "")
    sequents;

  buffer_add_line buf "status = solve(m)";
  buffer_add_line buf "println(getobjectivevalue(m))";
(*
  let julia_sos_declaration_v_domain = "syms " ^ julia_sos_syms_list ^ ";" in
  buffer_add_line buf julia_sos_declaration_v_domain; 
  buffer_add_line buf "";


  let julia_sos_dec_arg =
    "[" ^ julia_sos_of_list_infix_string julia_sos_of_v v_list "; " ^ "], " ^
      "[" ^ julia_sos_of_list_infix_string julia_sos_of_param param_list "; " ^ "]"
  in
  let julia_sos_initialization = 
    sdp_program_name ^ " = sosprogram(" ^ julia_sos_dec_arg ^ ");"
  in
  buffer_add_line buf julia_sos_initialization;
  buffer_add_line buf "";


  (* julia_sos_of_variable_declaration template_variables sdp_program_name :: *)
  List.iter
    (fun (s,sequent) ->
      buffer_add_line buf ("% "^ s);
      List.iter (buffer_add_line buf) (julia_sos_list_of_sdp_constraint sequent v_domain sos_deg sdp_program_name);
      buffer_add_line buf "")
    sequents;

  let str_obj = julia_sos_of_objective_function objective_func in
  let julia_sos_set_objective_function =
    sdp_program_name ^ " = sossetobj(" ^  sdp_program_name ^ ", obj);" in
  buffer_add_line buf "% setting options";
  buffer_add_line buf "option.solver = 'sdpt3';";
  buffer_add_line buf "option.params.maxit = 130;";
  buffer_add_line buf "% setting objective function";
  buffer_add_line buf ("obj = " ^ str_obj ^ ";");
  buffer_add_line buf julia_sos_set_objective_function;
  buffer_add_line buf "";

  let julia_sos_call_solver = sdp_program_name ^ " = sossolve(" ^ sdp_program_name ^ ", option)" in
  buffer_add_line buf "% calling solver";
  buffer_add_line buf julia_sos_call_solver;
  buffer_add_line buf "";

  let julia_sos_solve = "sol = sosgetsol(" ^ sdp_program_name ^ ", obj)" in
  buffer_add_line buf "% getting a bound";
  buffer_add_line buf julia_sos_solve;
*)
  Buffer.contents buf
