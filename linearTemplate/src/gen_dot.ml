open Pcfg
open Variable
open Coefficient
open Affine_term
open Polytope

let node_string l invariant term rewardmap = Printf.sprintf "%s [label=\"%s|inv: %s|term: %s|rew: %s\"];\n" (string_of_l l) (string_of_l l) (string_of_c_coef_linpred' @@ LM.find l invariant) (try string_of_c_coef_linpred' @@ LM.find l term with Not_found -> "") (string_of_float @@ LM.find l rewardmap)

let edge_string l l' label = Printf.sprintf "%s -> %s [label = \"%s\"];\n" (string_of_l l) (string_of_l l') label

let add_nodes buf l_domain invariant term rewardmap = List.iter (fun l -> Buffer.add_string buf @@ node_string l invariant term rewardmap) (l_list_of_l_domain l_domain)

let add_transitions buf pcfg_transition l_domain = List.iter (fun l -> match LM.find l pcfg_transition with 
  | A (a,l') -> Buffer.add_string buf @@ edge_string l l' (string_of_assgn' a)
  | P f -> List.iter (fun (l', p) -> Buffer.add_string buf @@ edge_string l l' (string_of_float p)) f
  | ND f -> List.iter (fun (g, l') -> Buffer.add_string buf @@ edge_string l l' (string_of_c_coef_linpred' g)) f
  ) (l_list_of_l_domain l_domain)

let generate_dot pcfg_transition l_domain invariant term rewardmap =
  let buf = Buffer.create (1024 * 1024) in
  Buffer.add_string buf "digraph pCFG {\n";
  Buffer.add_string buf "graph [rankdir = LR];\n";
  Buffer.add_string buf "node [shape = record];\n";
  add_nodes buf l_domain invariant term rewardmap;
  add_transitions buf pcfg_transition l_domain;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

