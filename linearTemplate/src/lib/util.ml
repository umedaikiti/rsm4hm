exception Err of string
exception Unsupported of string

let rec string_of_list_infix_string string_of_a ?string_of_empty_list:(string_of_empty_list="") list s =
    match list with
    | [] -> string_of_empty_list
    | hd :: [] -> string_of_a hd
    | hd :: tl -> string_of_a hd ^ s ^ string_of_list_infix_string string_of_a ~string_of_empty_list:string_of_empty_list tl s

let string_of_list (string_of_a: 'a -> string) (list: 'a list) : string =
  "[" ^ string_of_list_infix_string string_of_a list "; " ^ "]"  

let string_of_pair (string_of_a: 'a -> string) (string_of_b: 'b -> string) ((a, b): 'a * 'b) : string =
  "(" ^ string_of_a a ^ ", " ^ string_of_b b ^ ")"

let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []
let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []

let is_duplicate l =
  let rec go l = match l with
    | [] -> false
    | x :: xs -> (List.mem x xs) || go xs
  in go l  

let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let rec list_of_length_n n a =
  if n <= 0 then []
  else a :: list_of_length_n (n-1) a

let rec n_cartesian_product = function
  | [] -> [[]]
  | x :: xs ->
    let rest = n_cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)  

let rec cartesian list n =
  let n_of_list = list_of_length_n n list in
  n_cartesian_product n_of_list

let rec list_of_option_list: 'a option list -> 'a list = function
  | [] -> []
  | None :: rest -> list_of_option_list rest
  | Some elm :: rest -> elm :: (list_of_option_list rest)

