open Util
open Variable

(* === coefficient === *)
type 'a coef = Zero_coef | Unit_coef | Coef of 'a
let string_of_coef (string_of_a: 'a -> string) (coef : 'a coef) : string = match coef with
  | Zero_coef -> "Zero_coef"
  | Unit_coef -> "Unit_coef"
  | Coef a -> "Coef \"" ^ string_of_a a ^ "\""

let rec add_coef (add: 'a -> 'a -> 'a) (unit: 'a) (c1: 'a coef) (c2: 'a coef) : 'a coef =
  match c1, c2 with
  | Zero_coef, c | c, Zero_coef -> c
  | Unit_coef, c | c, Unit_coef -> add_coef add unit c (Coef unit)
  | Coef c1, Coef c2 -> Coef (add c1 c2)
let mult_coef (mult: 'a -> 'a -> 'a) (c1: 'a coef) (c2: 'a coef): 'a coef =
  match c1, c2 with
  | Zero_coef, _ | _, Zero_coef -> Zero_coef
  | Unit_coef, c | c, Unit_coef -> c
  | Coef a1, Coef a2 -> Coef (mult a1 a2)

(* c_coef *)
type c_coef = C_coef of float coef
let zero_c_coef : c_coef = C_coef Zero_coef
let unit_c_coef : c_coef = C_coef Unit_coef
let string_of_float_coef (coef : float coef) : string = string_of_coef string_of_float coef
let string_of_c_coef (C_coef coef: c_coef) : string =
  "C_coef (" ^ string_of_float_coef coef ^ ")"
let string_of_c_coef (C_coef coef: c_coef) : string =
  string_of_float_coef coef

let string_of_coef' (string_of_a: 'a -> string) (coef : 'a coef) : string = match coef with
  | Zero_coef -> "0"
  | Unit_coef -> "1"
  | Coef a -> string_of_a a
let string_of_float_coef' (coef : float coef) : string = string_of_coef' string_of_float coef
let string_of_c_coef' (C_coef coef: c_coef) : string = string_of_float_coef' coef


let c_coef_of_float (f: float) : c_coef = C_coef (Coef f)

let float_of_c_coef = function
  | C_coef Zero_coef -> 0.
  | C_coef Unit_coef -> 1.
  | C_coef (Coef f) -> f

let add_c_coef (C_coef c1: c_coef) (C_coef c2: c_coef): c_coef =
  C_coef (add_coef ( +. ) 1. c1 c2)
let mult_float_with_c_coef (f: float) (C_coef c: c_coef) : c_coef =
  match c with
  | Zero_coef -> C_coef Zero_coef
  | Unit_coef -> C_coef (Coef f)
  | Coef a -> C_coef (Coef (f *. a))
let mult_c_coef (C_coef c1: c_coef) (C_coef c2: c_coef): c_coef =
  C_coef (mult_coef ( *. ) c1 c2)
let div_c_coef (C_coef divident: c_coef) (C_coef divisor: c_coef): c_coef =
  match divisor, divident with
  | Zero_coef, _ -> raise Division_by_zero
  | Unit_coef, _ -> C_coef divident
  | Coef c, Zero_coef -> zero_c_coef
  | Coef c, Unit_coef -> C_coef (Coef (1. /. c))
  | Coef c, Coef a -> C_coef (Coef (a /. c))

let uminus_c_coef (c: c_coef) : c_coef = mult_float_with_c_coef (-1.) c

(* p_coef *)
type p_coef = P_coef of (float * param) list coef
let zero_p_coef : p_coef = P_coef Zero_coef
let unit_p_coef : p_coef = P_coef Unit_coef
let string_of_float_param_list (list: (float * param) list) : string =
  string_of_list (string_of_pair string_of_float string_of_param) list
let string_of_float_param_list_coef (coef : (float * param) list coef) : string = string_of_coef string_of_float_param_list coef
let string_of_p_coef (P_coef coef: p_coef) : string = "P_coef (" ^ string_of_float_param_list_coef coef ^ ")"
let string_of_float_param_list' (list: (float * param) list) : string = String.concat "+" @@ List.map (fun (f, p) -> "(" ^ string_of_float f ^ ") * " ^ string_of_param p) list
let string_of_float_param_list_coef' (coef : (float * param) list coef) : string = string_of_coef' string_of_float_param_list' coef
let string_of_p_coef' (P_coef coef: p_coef) : string = "(" ^ string_of_float_param_list_coef' coef ^ ")"

(* P_coef [Coef (2., "p"); Coef (3., "p")] -> P_coef [Coef (5., "p")] *)
let normalize_p_coef (P_coef coef as p_coef: p_coef) : p_coef =
  match coef with
  | Zero_coef -> p_coef
  | Unit_coef -> p_coef
  | Coef fp_list ->
    P_coef (Coef (remove_duplicates @@
    List.map (fun (_, p_arg) ->
        let pred (_, p) = p = p_arg in
        let float_list = fst @@ List.split @@ List.filter pred fp_list in
        (List.fold_left (fun b f -> b +. f) 0. float_list, p_arg)
    )
    fp_list))

let float_param_list_of_float (f: float) : (float * param) list = [(f, empty_param)]
let float_param_list_of_param (p: param) : (float * param) list = [(1., p)]
let p_coef_of_float (f: float) : p_coef = normalize_p_coef @@ P_coef (Coef (float_param_list_of_float f))
let p_coef_of_param (p: param) : p_coef = normalize_p_coef @@ P_coef (Coef (float_param_list_of_param p))
let p_coef_of_float_param (f, p: float * param) : p_coef = normalize_p_coef @@ P_coef (Coef [f, p])
let p_coef_of_float_param_list (fp_list: (float * param) list) : p_coef = normalize_p_coef @@ P_coef (Coef fp_list)
let p_coef_of_c_coef (C_coef c: c_coef) : p_coef =
  normalize_p_coef @@
  match c with
  | Zero_coef -> P_coef Zero_coef
  | Unit_coef -> P_coef Unit_coef
  | Coef a -> P_coef (Coef [(a , empty_param)])

let float_param_list_of_p_coef = function
  | P_coef Zero_coef -> float_param_list_of_float 0.
  | P_coef Unit_coef -> float_param_list_of_float 1.
  | P_coef (Coef f) -> f

let rec bop_on_float_param_list (bop: float -> float -> float) (fp_list1: (float * param) list) (fp_list2: (float * param) list) : (float * param) list =
  let folder =
    (fun (b: (float * param) list) ((f_elm, p_elm) as fp_elm: float * param) ->
       let pred (_, p) = p = p_elm in
       let collapse : (float * param) list -> (float * param) option = function
         | [] -> None
         | ((f1, p1) as fp_hd) :: rest ->
           Some (List.fold_left
                   (fun (b, _) (elm, _) -> (bop b elm, p1))
                   fp_hd rest)
       in
       match collapse @@ List.filter pred (fp_elm :: b) with
       | None -> fp_elm :: b
       | Some a -> a :: (List.filter (fun x -> not @@ pred x) b)) in
  List.fold_left
    folder
    (List.fold_left folder [] fp_list1)
  fp_list2
let add_float_param_list (fp_list1: (float * param) list) (fp_list2: (float * param) list) : (float * param) list =
  bop_on_float_param_list ( +. ) fp_list1 fp_list2
(** implementation note: normalization is done in add_float_param_list *)
let add_p_coef (P_coef p1: p_coef) (P_coef p2: p_coef): p_coef =
  normalize_p_coef @@ P_coef (add_coef add_float_param_list [1., empty_param] p1 p2)

let mult_float_with_float_param_list (f: float) (float_param_list: (float * param) list) : (float * param) list =
  List.map (fun (f_elm, p_elm) -> (f_elm *. f, p_elm)) float_param_list
let mult_float_with_p_coef (f: float) (P_coef c: p_coef) : p_coef =
  normalize_p_coef @@
  match c with
  | Zero_coef -> P_coef Zero_coef
  | Unit_coef ->
    P_coef (Coef (mult_float_with_float_param_list f [(1., empty_param)]))
  | Coef a ->
    P_coef (Coef (mult_float_with_float_param_list f a))
let mult_c_coef_with_p_coef (C_coef coef: c_coef) (pc: p_coef) : p_coef =
  normalize_p_coef @@
  match coef with
  | Zero_coef -> P_coef Zero_coef
  | Unit_coef -> pc
  | Coef a -> mult_float_with_p_coef a pc

let mult_float_param_list (fp_list1: (float * param) list) (fp_list2: (float * param) list): (float * param) list =
  bop_on_float_param_list ( *. ) fp_list1 fp_list2
let mult_p_coef (P_coef p1: p_coef) (P_coef p2: p_coef): p_coef =
  normalize_p_coef @@
  P_coef (mult_coef mult_float_param_list p1 p2)

let uminus_p_coef (p: p_coef) : p_coef = normalize_p_coef @@ mult_float_with_p_coef (-1.) p
let scale_p_coef f p = normalize_p_coef @@ mult_float_with_p_coef f p
