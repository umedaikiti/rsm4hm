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
open Gen_dot

let usage_msg =  Filename.basename Sys.argv.(0) ^ " [options] file"

let _ =
  let out_file = ref "" in
  let order = ref 2 in
  let in_file = ref "" in
  let debug = ref false in
  Arg.parse [
    ("-o", Set_string out_file, "FILENAME  Output into FILENAME");
    ("-order", Set_int order, "K  Use ranking supermartingale for K-th moment (default: K = 2)");
    ("-debug", Set debug, " Set debug mode on")
  ] (fun s -> match !in_file with
      | "" -> in_file := s
      | _ -> failwith "one file at a time") usage_msg;
  let in_file = match !in_file with
    | "" -> failwith "no input file"
    | s -> s in
  let out_file = match !out_file with
    | "" -> (try Filename.chop_extension (Filename.basename in_file) with _ -> in_file) ^ ".mod"
    | s -> s in
  let order = if !order <= 0 then failwith "order must be positive" else !order in
  let debug = !debug in
  let ic = open_in in_file in
  try 
    let prog = Parser.program Lexer.main (Lexing.from_channel ic) in
    let (pcfg_transition, l_domain, v_domain), init_config, invariant, term_linpredmap, rewardmap = eval prog in
    if debug then
    ((*print_string "locations:\n";
    print_endline @@ string_of_l_domain l_domain;*)
    let l_init, _ = init_config in print_string "init_location: ";
    print_endline @@ string_of_l l_init;
    (*print_string "transition:\n";
    print_endline @@ string_of_list string_of_f @@ List.map (fun l -> LM.find l pcfg_transition) (l_list_of_l_domain l_domain);*)
    print_string "variables:\n";
    print_endline @@ Variable.string_of_v_domain v_domain;
    (*print_string "invariant:\n";
    print_endline @@ string_of_linpredmap invariant;*)
    print_string "term_linpredmap:\n";
    print_endline @@ string_of_linpredmap term_linpredmap;
    print_endline @@ generate_dot pcfg_transition l_domain invariant term_linpredmap rewardmap)
    else ();
    let problem: reachability_problem =
      (pcfg_transition, l_domain, v_domain),
      order,
      init_config,
      invariant,
      term_linpredmap,
      rewardmap in
    let progmath = progmath_of_reachability_problem problem debug in
    ((*print_string progmath;*)
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

