open Util
open Variable
open Coefficient
open Polynomial
open Semialg
open Pcfg
open Sequent

let generate_param_list (v_domain : v_domain) (deg: int) (id: string) (l: l): param list =
  let monomial_list = possible_monomial_list v_domain deg in
  let string_of_v_int_list v_int_list =
  let string_of_v_int (v, int) =
    if monomial_of_v_int v int = empty_monomial then
      ""
    else
      string_of_v v ^ string_of_int int
  in
    List.fold_left (fun b elm -> b ^ string_of_v_int elm) "" v_int_list
  in
  let param_name monomial l = string_of_v_int_list (v_int_list_of_monomial monomial) ^ string_of_l l ^ id in
  List.map (fun m -> param_of_string (param_name m l)) monomial_list

let generate_p_coef_polynomial (v_domain : v_domain) (param_list: param list) (deg: int): p_coef polynomial =
  let monomial_list = possible_monomial_list v_domain deg in
  p_coef_polynomial_of_p_coef_monomial_list @@
  (List.map2 (fun p m -> p_coef_of_param p, m) param_list monomial_list)

let generate_polynomial_template (l_domain : l_domain) (v_domain : v_domain) (deg: int) (id: string) : template * param_domain =
  let l_list = l_list_of_l_domain l_domain in
  let (template, param_list)=
    List.fold_left
    (fun (template, param_list) l ->
      let new_param_list = generate_param_list v_domain deg id l in
      (LM.add l (generate_p_coef_polynomial v_domain new_param_list deg) template,
       new_param_list@param_list))
    (LM.empty,[])
    l_list 
  in
  (template, param_domain_of_param_list param_list)



