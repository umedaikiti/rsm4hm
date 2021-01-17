(* open Polynomial
 * open Semialg
 * open Poly_pcfg
 * open Poly_supermartingale
 * open Sdp_constraint
 * open Sostools *)

open Util
open Pcfg
open Progmath
open Eval


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
  let out_file =
    match search_argv "-o"  argvtl with
    | None -> (try Filename.chop_extension (Filename.basename in_file) with _ -> in_file) ^ ".mod" 
    | Some o -> o in
  let gamma =
    match search_argv "-gamma"  argvtl with
    | None -> print_endline "gamma = 0.9"; 0.9
    | Some gamma_s -> float_of_string gamma_s in
  let ic = open_in in_file in
  if gamma <= 0. || 1.<= gamma then Printf.eprintf "Error: gamma should be: 0 < gamma < 1.\n" else
  let tmpltype =
    if List.mem "-exp" argvtl || List.mem "-exponential" argvtl then EXP else
    if List.mem "-lin" argvtl || List.mem "-linear" argvtl then LIN else
    (print_endline "Template: inear"; LIN) in
  try 
    let prog = Parser.program Lexer.main (Lexing.from_channel ic) in
    let (pcfg_transition, l_domain, v_domain), init_config, invariant, term_linpredmap, rewardmap = eval prog in
    let problem: reachability_problem =
      (pcfg_transition, l_domain, v_domain),
      tmpltype,
      init_config,
      invariant,
      term_linpredmap in
    let progmath = progmath_of_reachability_problem problem gamma in
    (print_string progmath;
     let oc = open_out out_file in
     try
       Printf.fprintf oc "%s" progmath;
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

