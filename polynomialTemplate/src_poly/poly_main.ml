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

let usage_msg =  Filename.basename Sys.argv.(0) ^ " [options] file"

let _ =
  let out_file = ref "" in
  let order = ref 2 in
  let in_file = ref "" in
  let julia = ref false in
  let template_deg = ref 2 in
  let sos_deg = ref 1 in
  let sdp_prog_name = ref "" in
  Arg.parse [
    ("-o", Set_string out_file, "FILENAME  Output into FILENAME");
    ("-sdpprogname", Set_string sdp_prog_name, "PROGNAME  Set SDP program name to PROGNAME");
    ("-deg", Set_int template_deg, "DEG  Use polynomial templates of degree DEG");
    ("-order", Set_int order, "K  Use ranking supermartingale for K-th moment (default: K = 2)");
    ("-sosdeg", Set_int sos_deg, "SOSDEG  Use sos degree = SOS");
    ("-julia", Set julia, "(not implemented) Enable julia output");
  ] (fun s -> match !in_file with
      | "" -> in_file := s
      | _ -> failwith "one file at a time") usage_msg;
  let in_file = match !in_file with
    | "" -> failwith "no input file"
    | s -> s in
  let julia = !julia in
  let template_deg = if !template_deg < 1 then failwith "template degree must be nonnegative" else !template_deg in
  let order = if !order <= 0 then failwith "order must be positive" else !order in
  let file_name = try Filename.chop_extension (Filename.basename in_file) with _ -> in_file in
  let out_file = match !out_file with
    | "" -> file_name ^ "_pp_poly_deg" ^ string_of_int template_deg ^ "_order" ^ string_of_int order ^ if julia then ".jl" else ".m"
    | s -> s in
  let sos_deg = !sos_deg in (* TODO check *)
  let sdp_prog_name = if !sdp_prog_name = "" then file_name else !sdp_prog_name in
  if Array.get Sys.argv 1 = "-distr_usage" then print_endline distributions_usage else
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

