open Util
open Variable
open Coefficient
open Matrix
open Pcfg
open Lp_constraint

type progmath = string

let progmath_of_v = string_of_v
let progmath_of_param = string_of_param
let progmath_of_float (f: float) : progmath =
  let fs = string_of_float f in
  if String.get fs (String.length fs - 1) = '.' then
    "(" ^ fs ^ "0" ^ ")"
  else
    "(" ^ fs ^ ")"
let progmath_of_int = string_of_int

let progmath_of_c_coef : c_coef -> progmath =
  fun c_coef -> progmath_of_float @@ float_of_c_coef c_coef

let progmath_of_p_coef : p_coef -> progmath =
  fun p_coef ->
    let progmath_of_float_param (f, p) =
      if p = empty_param then progmath_of_float f
      else progmath_of_float f ^ "*" ^ progmath_of_param p
    in
    let float_param_list = float_param_list_of_p_coef p_coef in
  if List.length float_param_list = 0 then "0" else    
    "(" ^ string_of_list_infix_string progmath_of_float_param float_param_list "+" ^ ")"

let put_plus lst = string_of_list_infix_string (fun x -> x) lst "+"
let put_times lst = string_of_list_infix_string (fun x -> x) lst "*"

let is_trivially_true_lp_constraint (((a_mat, b_vec), _) : lp_constraint) : bool =
  List.exists
    (fun (a_row, b_elem) ->
      List.for_all (fun a_elem -> float_of_c_coef a_elem = 0.) a_row &&
      float_of_c_coef b_elem < 0.)
    (List.combine a_mat b_vec)

let progmath_of_c_coef_vec_string_list_mult (a_row: c_coef vector) (ys: string list) : string =
  if List.length a_row = 0 then "0" else
  let tmp =
    List.map
      (fun (a_elm, y_elm) -> progmath_of_c_coef a_elm ^ " * " ^ y_elm)
      (try (List.combine a_row ys) with _ -> failwith "List.combine in progmath_of_c_coef_vec_string_list_mult")
  in
  put_plus tmp

let cnt_id = ref 0
let such_that_y_list_of_output_elm (((a_mat, b_vec), (c_vec, d_sca)) : lp_constraint) : string list * string list =
  let elm_id = "a" ^ string_of_int !cnt_id in
  let () = (cnt_id := !cnt_id + 1) in

  let generate_y (cnt_id: string) (length: int) : string list =
    let ret = ref [] in
    for i = 1 to length do
      ret := !ret @ ["y" ^ string_of_int i ^ cnt_id]
    done;
    !ret in
  let y = generate_y (string_of_int !cnt_id) (List.length b_vec) in
(*print_string ("\n"^ string_of_int (List.length b_vec) ^ "::");
  string_of_vector string_of_c_coef b_vec;
  print_string "\n";
  List.iter print_string y;*)

  (* non-negativeness y >= 0 *)
  let get_pos_lines elm_id y = List.map
      (fun y_elm ->
         let header =
           let title = elm_id ^ "_" ^ y_elm ^ "_pos" in
           "s.t. " ^  title ^ ": " in
         let body = y_elm ^ " >= 0;" in
         header ^ body
      )
      y in

  (* equality A^t y = c *)
  let get_eq_lines elm_id y =
    let a_t_mat = transpose_matrix (vector_size c_vec) a_mat in
    let cnt = ref 0 in
    List.map
      (fun (a_row, c_elm) ->
         let header =
           let title = elm_id ^ "_" ^ string_of_int !cnt ^ "_eq" in
           "s.t. " ^ title ^ ": " in
         let body =
           let a_t_y = progmath_of_c_coef_vec_string_list_mult a_row y in
           let c = progmath_of_p_coef c_elm in
           a_t_y ^ " = " ^ c ^ ";" in
         cnt := !cnt + 1; header ^ body
      )
      (try List.combine a_t_mat c_vec with _ -> failwith ("List.combine in get_eq_lines" ^ string_of_matrix string_of_c_coef a_t_mat ^ " vs " ^ string_of_vector string_of_p_coef c_vec)) in

  (* inequality  b^t y <= d *)
  let get_ineq_lines elm_id y =
    let header =
      let title = elm_id ^ "_ineq" in
      "s.t. " ^ title ^ ": " in
    let body =
      let b_t_y = progmath_of_c_coef_vec_string_list_mult b_vec y in
      let d = progmath_of_p_coef d_sca in
      b_t_y ^ " <= " ^ d ^ ";" in
    [header ^ body] in

  (List.fold_left
     (fun b f -> b @ f elm_id y)
     []
     [get_pos_lines; get_eq_lines; get_ineq_lines],
   y)

let string_of_lp_constraint ((a, b), (c, d)) = string_of_matrix string_of_c_coef' a ^ string_of_vector string_of_c_coef' b

let progmath_of_mat_vecs (lp_constraints: lp_constraint list) : string * string list=
  let _ = (cnt_id := 0) in
  let such_that, y =
    let progmath_of_such_that_seed_seed such_that_seed_seed =
      List.fold_left (fun base elm -> base ^ elm ^ "\n") "" such_that_seed_seed in
    let progmath_of_such_that_seed such_that_seed =
      List.fold_left
        (fun base output_elm -> base ^ "\n" ^ progmath_of_such_that_seed_seed output_elm)
        ""
        such_that_seed in
    let such_that_seed, y_seed = List.split (List.map such_that_y_list_of_output_elm lp_constraints) in
    progmath_of_such_that_seed such_that_seed, List.flatten y_seed in
  (such_that, y)

let progmath_of_reachability_problem (problem: reachability_problem) debug : progmath =
  let lp_constraints, template_var_list, objective_func_string = lp_seeds_of_reachability_problem problem debug in
  let lp_constraints = List.filter (fun x -> not @@ is_trivially_true_lp_constraint x) lp_constraints in 
  (*f debug then print_string @@ string_of_list string_of_lp_constraint lp_constraints else ();*)
  let such_that, ys = progmath_of_mat_vecs lp_constraints in
  let var_generate x_list = List.fold_left (fun base x -> base ^ "var " ^ x ^ ";\n") "" x_list in
  let _, order, _, _, _, _ = problem in
  let header = "# moment of order " ^ string_of_int order ^ "\n" in
  let vars_string = var_generate (ys @ template_var_list) in
  header ^ vars_string ^ "\n" ^ "minimize moment_bound: " ^ objective_func_string ^ ";\n" ^ such_that ^ "\n" ^ "end;" ^ "\n"
 
