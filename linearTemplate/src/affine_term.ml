open Util
open Variable
open Coefficient

exception Err of string
exception Unsupported of string

(* === term over coefficient === *)
type 'coef term = ('coef * v) list
let string_of_term (string_of_coef: 'coef -> string) (term: 'coef term) : string =
  "(term: " ^ string_of_list (string_of_pair string_of_coef string_of_v) term ^ ")"
let string_of_term' (string_of_coef: 'coef -> string) (term: 'coef term) : string =
  String.concat "+" @@ List.map (fun (c, v) -> "(" ^ string_of_coef c ^ ") * " ^ string_of_v v) term
let unit_term : 'coef term = []
let term_of_coef (coef: 'coef) : 'coef term = [(coef, empty_v)]
let term_of_v (unit_coef: 'coef) (v: v) : 'coef term = [(unit_coef, v)]
let term_of_coef_v_list (c_list: 'coef list) (v_list: v list) : 'coef term = List.combine c_list v_list

let coef_list_of_term (term: 'coef term) : 'coef list = fst (List.split term)
let v_list_of_term (term: 'coef term) : v list = snd (List.split term)



let normalize_term (add_coef: 'coef -> 'coef -> 'coef) (term : 'coef term) : 'coef term =
  let rec add_monomial_to_term  (resulting_term: 'coef term) (term: 'coef term) ((coef, v): 'coef * v) =
    match term with
    | [] -> (coef,v)::resulting_term
    | (coef',v')::term' ->
       if v = v' then
         (add_coef coef coef', v)::(resulting_term @ term')
       else
         add_monomial_to_term ((coef',v')::resulting_term) term' (coef, v)
  in
  List.fold_left (add_monomial_to_term []) [] term 


let add_term (add_coef: 'coef -> 'coef -> 'coef) (t1: 'coef term) (t2: 'coef term) : 'coef term =
  normalize_term add_coef (t1@t2)
(*
  let coef_list1, v_list1 = List.split t1 in
  let coef_list2, v_list2 = List.split t2 in
  let rec sub (coef_list1: 'coef list) (v_list1: v list) (coef_list2: 'coef list) (v_list2: v list) : ('coef list * v) list =
    match coef_list1, v_list1 with
    | [], [] ->  List.map2 (fun c v -> ([c], v)) coef_list2 v_list2
    | c_current::c_rest, v_current :: v_rest ->
      let c_list, _ = List.split (List.filter (fun (c,v) -> v = v_current) (List.combine coef_list2 v_list2))  in
      let new_c_list2, new_v_list2 = List.split (List.filter (fun (c,v) -> not (v = v_current)) (List.combine coef_list2 v_list2)) in
      ((c_current :: c_list), v_current) :: sub c_rest v_rest new_c_list2 new_v_list2
    | _ -> raise (Err "invalid argument in add_term")
  in
  let tmp = sub coef_list1 v_list1 coef_list2 v_list2 in
  List.map (fun (c_list,v)  -> (List.fold_left add_coef zero_coef c_list, v)) tmp
 *)

let uminus_term (uminus_coef: 'coef ->'coef) (term: 'coef term) : 'coef term =
  List.map (fun (coef, v) -> (uminus_coef coef, v)) term 

(* c_coef term*)
let string_of_c_coef_term : c_coef term -> string = string_of_term string_of_c_coef
let string_of_c_coef_term' : c_coef term -> string = string_of_term' string_of_c_coef'


let normalize_c_coef_term (cv_list : c_coef term) : c_coef term =
  normalize_term add_c_coef cv_list
(*
  remove_duplicates @@
  List.map (fun (_, v_arg) ->
      let pred (_, v) = (v = v_arg) in
      let coef_list = fst @@ List.split @@ List.filter pred cv_list in
      (List.fold_left (fun b c -> add_c_coef b c) zero_c_coef coef_list, v_arg)
    )
    cv_list
 *)
let c_coef_term_of_float (f: float) : c_coef term = normalize_c_coef_term @@ term_of_coef (c_coef_of_float f)
let c_coef_term_of_c_coef (c: c_coef) : c_coef term = normalize_c_coef_term @@ term_of_coef c
let c_coef_term_of_v (v: v) : c_coef term = normalize_c_coef_term @@ term_of_v unit_c_coef v
let c_coef_term_of_c_coef_v_list (cv_list: (c_coef * v) list) : c_coef term = normalize_c_coef_term @@ cv_list
let c_coef_v_list_of_c_coef_term c_term = c_term
let c_coef_list_of_c_coef_term c = fst @@ List.split @@ c_coef_v_list_of_c_coef_term c
let v_list_of_c_coef_term c = snd @@ List.split @@ c_coef_v_list_of_c_coef_term c

let zero_c_coef_term: c_coef term = [(zero_c_coef, empty_v)]
let unit_c_coef_term: c_coef term = [(unit_c_coef, empty_v)]

let add_c_coef_term (t1: c_coef term) (t2: c_coef term) : c_coef term =
  normalize_c_coef_term @@ add_term add_c_coef t1 t2
let uminus_c_coef_term (term: c_coef term) : c_coef term = normalize_c_coef_term @@ uminus_term uminus_c_coef term
let scale_c_coef_term (f: float) (c_t: c_coef term) : c_coef term =
  normalize_c_coef_term @@ 
  List.map (fun (c_coef_elm, v_elm) -> (mult_c_coef (c_coef_of_float f) c_coef_elm, v_elm)) c_t

(* p_coef term*)
let string_of_p_coef_term : p_coef term -> string = string_of_term string_of_p_coef
let string_of_p_coef_term' : p_coef term -> string = string_of_term' string_of_p_coef'

let normalize_p_coef_term (pv_list : p_coef term) : p_coef term =
  normalize_term add_p_coef pv_list
(*
  remove_duplicates @@
  List.map (fun (_, v_arg) ->
      let pred (_, v) = v = v_arg in
      let coef_list = fst @@ List.split @@ List.filter pred pv_list in
      (List.fold_left (fun b c -> add_p_coef b c) zero_p_coef coef_list, v_arg)
    )
    pv_list
 *)
let p_coef_term_of_float (f: float) : p_coef term = normalize_p_coef_term @@ term_of_coef (p_coef_of_float f)
let p_coef_term_of_p_coef (p: p_coef) : p_coef term = normalize_p_coef_term @@ term_of_coef p
let p_coef_term_of_v (v: v) : p_coef term = normalize_p_coef_term @@ term_of_v unit_p_coef v
let p_coef_term_of_p_coef_v_list (pv_list: (p_coef * v) list) : p_coef term = normalize_p_coef_term @@ pv_list
let p_coef_v_list_of_p_coef_term p_term = p_term
let p_coef_list_of_p_coef_term p = fst @@ List.split @@ p_coef_v_list_of_p_coef_term p
let v_list_of_p_coef_term p = snd @@ List.split @@ p_coef_v_list_of_p_coef_term p
let zero_p_coef_term: p_coef term = [(zero_p_coef, empty_v)]
let unit_p_coef_term: p_coef term = [(unit_p_coef, empty_v)]

let add_p_coef_term (t1: p_coef term) (t2: p_coef term) : p_coef term =
  normalize_p_coef_term @@
  add_term add_p_coef t1 t2
let uminus_p_coef_term (term: p_coef term) : p_coef term = normalize_p_coef_term @@ uminus_term uminus_p_coef term
let scale_p_coef_term (f: float) (p_t: p_coef term) : p_coef term =
  normalize_p_coef_term @@ 
  List.map (fun (p_coef_elm, v_elm) -> (scale_p_coef f p_coef_elm, v_elm)) p_t


let substitute_v_with_v_in_coef_term (term: 'coef term) (v : v) (v' : v): 'coef term =
  let rec get_coef_and_rewrite (resulting_term : 'coef term) (term : 'coef term) : 'coef term =
    match term with
    | [] -> resulting_term
    | (coef,v'')::term' ->
       if v = v'' then
         (coef,v')::(resulting_term @ term')
       else
         get_coef_and_rewrite ((coef,v'')::resulting_term) term'
  in
  get_coef_and_rewrite unit_term term  

(* lin_expr, v_i, term |-> term(x_i <- lin_expr) *)
let substitute_v_with_c_coef_term_in_p_coef_term (c_t: c_coef term) (v_i: v) (p_t:p_coef term) : p_coef term =
  let p_t = normalize_p_coef_term p_t in
  let rec get_p_coef_v_i_and_remove (resulting_term : p_coef term) (term : p_coef term) : p_coef * p_coef term =
    match term with
    | [] -> raise Not_found
    | (p_coef',v')::term' ->
       if v' = v_i then
         (p_coef', term'@resulting_term)
       else
         get_p_coef_v_i_and_remove ((p_coef',v')::resulting_term) term'
  in
  try
    let (p_coef_v_i, p_coef_term_other) = get_p_coef_v_i_and_remove unit_term p_t in
    let partially_composed_term =
      List.map
        (fun (c_coef_elm, v_elm) -> (scale_p_coef (float_of_c_coef c_coef_elm) p_coef_v_i, v_elm))
        c_t in
    add_p_coef_term partially_composed_term p_coef_term_other 
  with Not_found -> p_t 

