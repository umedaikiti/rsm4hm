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


let _ =
  if Array.length Sys.argv <= 1 then Printf.eprintf "Error: please specify an input file.\n" else
  let in_file = Array.get Sys.argv 1 in
  let rec search_argv s l = match l with
    | x::y::l' ->
       (match x with
        | s' when s' = s -> Some y
        | s' -> search_argv s (y::l'))
    | _ -> None in
  let argvtl = List.tl @@ List.tl @@ Array.to_list Sys.argv in
  let out_file =
    match search_argv "-o" @@  argvtl with
    | None -> (try Filename.chop_extension (Filename.basename in_file) with _ -> in_file) ^ ".mod"
    | Some o -> o in
  let order =
    match search_argv "-order"  argvtl with
    | None -> print_endline "order = 2"; 2
    | Some order_s -> int_of_string order_s in
  let debug = List.mem "-debug" argvtl in
  if order <= 0  then Printf.eprintf "Error: order should be positive.\n" else
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

