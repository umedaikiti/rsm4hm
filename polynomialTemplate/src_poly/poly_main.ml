open Util
open Variable
open Pcfg
open Sostools
open Julia_sos
open Eval

(** == naming convention of output matlab file ==
    (convention policy: <source>_<program type>)
    <example_name>_<pcfg|pp>_<aff|poly_deg?>.m
*)

(** == naming convention of sosprogram name ==
    (convention policy: <source>_<program type>)
    <example_name>_<pcfg|pp>
*)

let _ =
  if Array.length Sys.argv <= 1 then Printf.eprintf "Error: please specify an input file.\n" else
  if Array.get Sys.argv 1 = "-distr_usage" then print_endline distributions_usage else
  let in_file = Array.get Sys.argv 1 in
  let rec search_argv s l = match l with
    | x::y::l' ->
       (match x with
        | s' when s' = s -> Some y
        | s' -> search_argv s (y::l'))
    | _ -> None in
  let argvtl = List.tl @@ List.tl @@ Array.to_list Sys.argv in
  let template_deg = 
    match search_argv "-deg"  argvtl with
    | None -> print_endline "template degree = 2"; 2
    | Some deg -> int_of_string deg in
  let julia = List.mem "-julia" argvtl in
  let order =
    match search_argv "-order"  argvtl with
    | None -> print_endline "order = 2"; 2
    | Some order_s -> int_of_string order_s in
  let file_name = try Filename.chop_extension (Filename.basename in_file) with _ -> in_file in
  let out_file =
    match search_argv "-o" @@  argvtl with
    | None -> file_name ^ "_pp_poly_deg" ^ string_of_int template_deg ^ "_order" ^ string_of_int order ^ if julia then ".jl" else ".m" 
    | Some o -> o in
  let sos_deg = 
    match search_argv "-sosdeg"  argvtl with
    | None -> print_endline "sos degree = 1"; 1
    | Some deg -> int_of_string deg in
  let sdp_prog_name = 
    match search_argv "-sdpprogname"  argvtl with
    | None -> print_endline ("sdp prog name = " ^ file_name); file_name 
    | Some name -> name in
  if order <= 0 then Printf.eprintf "Error: order should be positive.\n" else
  if template_deg < 1 then Printf.eprintf "Error: template degree should be: 0 <= deg.\n" else
(*  let tmpltype =
    if List.mem "-exp" argvtl || List.mem "-exponential" argvtl then EXP else
    if List.mem "-lin" argvtl || List.mem "-linear" argvtl then LIN else
    (print_endline "Template: inear"; LIN) in*)
  let ic = open_in in_file in
  try 
    let prog: Syntax.prog = Parser.program Lexer.main (Lexing.from_channel ic) in
    let (pcfg_transition, l_domain, v_domain), init_config, invariant, term_linpredmap, rewardmap = eval prog in
    let problem: reachability_problem =
      (pcfg_transition, l_domain, v_domain),
      init_config,
      invariant,
      term_linpredmap,
      rewardmap,
      order in
    let output_string = if julia then julia_sos_of_reachability_problem problem template_deg sos_deg sdp_prog_name else sostools_of_reachability_problem problem template_deg sos_deg sdp_prog_name in
    (let oc = open_out out_file in
     try
       Printf.fprintf oc "%s" output_string;
       flush oc;
       close_out oc
     with e ->
       close_out_noerr oc;
       raise e);
    flush stdout;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e

