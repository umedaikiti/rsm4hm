open Util


(* === variables === *)
type 'a var = Empty_var | Var of 'a | Temporary of 'a
let string_of_string_var (v: string var) : string = match v with
  | Empty_var -> ""
  | Var a -> a
  | Temporary a -> "_" ^ a


type 'var valuation = ('var * float) list
(*
let normalize_v_valuation v_valuation =
  if is_duplicate v_valuation then
    failwith "v_valuation is duplicate. (I don't know which to assign)"
  else
    v_valuation
 *)
let value_of v valuation = List.assoc v valuation
let update_of_valuation valuation v f = (v, f) :: List.remove_assoc v valuation
let valuation_of_var_float_list x = x
let var_float_list_of_valuation valuation = valuation
(* let string_of_valuation valuation = string_of_list (string_of_pair string_of_v string_of_float) valuation *)


(* v *)
type v = V of string var

let normalize_v (V v) =
  match v with
  | Empty_var -> V v
  | Var a | Temporary a ->
    if a = "" then V Empty_var
    else V v
let empty_v : v = V Empty_var
let is_empty_v (V v) = (v=Empty_var)
let string_of_v (V v: v) : string = string_of_string_var v
let v_of_string (string: string) : v =
  normalize_v @@ V (Var string)
let v_of_string_temporary (string: string) : v =
  normalize_v @@ V (Temporary string)
let compare_v v1 v2 = compare (string_of_v v1) (string_of_v v2)

type v_domain = v list
let normalize_v_domain  (v_domain: v_domain) : v_domain =
  remove_duplicates @@
  (List.map (fun (V v) ->
       match v with
       | Empty_var -> V v
       | Var a | Temporary a ->
         if a = "" then empty_v
         else V v)
      v_domain)
let v_domain_of_v_list v_list = normalize_v_domain v_list
let v_list_of_v_domain v_domain = normalize_v_domain v_domain
let string_of_v_domain v_domain = string_of_list string_of_v v_domain





(* param *)
type param = Param of string var
let normalize_param (Param p) =
  match p with
  | Empty_var  -> Param p
  | Var a | Temporary a ->
    if a = "" then Param Empty_var
    else Param p
let empty_param : param = Param Empty_var
let is_empty_param (Param param) = (param = Empty_var)
let string_of_param (Param p: param) : string = string_of_string_var p
let param_of_string (string: string) : param =
  normalize_param @@ Param (Var string)
let compare_param p1 p2 = compare (string_of_param p1) (string_of_param p2)


type param_domain = param list
let normalize_param_domain  (p_domain: param_domain) : param_domain =
  remove_duplicates @@
  (List.map (fun (Param p) ->
       match p with
       | Empty_var -> Param p
       | Var a | Temporary a ->
         if a = "" then empty_param
         else Param p)
      p_domain)
let param_domain_of_param_list param_list = normalize_param_domain param_list
let param_list_of_param_domain param_domain = normalize_param_domain param_domain
let string_of_param_domain param_domain = string_of_list string_of_param param_domain
