(* Multivariable: Term of int * char * int -> no need for var ref, compare char throughout *)
type polyExpr =
  | Term    of int * int 
  | Plus    of polyExpr list
  | Times   of polyExpr list ;;

let var : char ref = ref '\x00' ;;

(* Function to traslate betwen AST expressions to polyExpr expressions *)
let rec from_expr (expr : Expr.expr) : polyExpr =
  match expr with
  | Num i -> Term (i, 0)
  | Var v -> if !var = '\x00' then var := v; Term (1, 1)
  | Add (e1, e2) -> Plus [from_expr e1; from_expr e2]
  | Sub (e1, e2) -> Plus [from_expr e1; from_expr (Neg e2)]
  | Mul (e1, e2) -> Times [from_expr e1; from_expr e2]
  | Pos e -> from_expr e
  | Neg e -> Times[Term (-1, 0); from_expr e]
  | Pow (e, i) -> (
    let poly_expr : polyExpr = from_expr e in
      let rec pow_exp (pow : int) (acc : polyExpr list) : polyExpr list =
        if pow >= 0 then
          match pow with 
          | 0 -> [Term ((if e = Num 0 then 0 else 1), 0)]
          | 1 -> poly_expr :: acc
          | _ -> pow_exp (pow - 1) (poly_expr :: acc)
        else [Term (0, 0)] in
      Times (pow_exp i [])) ;;

let print_term (c : int) (p : int) : unit =
  match c with
  | -1 -> (
    match p with
    | 0 -> Printf.printf "(-1)"
    | 1 -> Printf.printf "(-%c)" !var
    | _ -> Printf.printf "(-%c^%d)" !var p)
  | 0 -> Printf.printf "0"
  | 1 -> (
    match p with
    | 0 -> Printf.printf "1"
    | 1 -> Printf.printf "%c" !var
    | _ -> Printf.printf "%c^%d" !var p)
  | _ -> 
    match p, c > 1 with
    | 0, true -> Printf.printf "%d" c
    | 0, false -> Printf.printf "(%d)" c
    | 1, true -> Printf.printf "%d%c" c !var
    | 1, false -> Printf.printf "(%d%c)" c !var
    | _, true -> Printf.printf "%d%c^%d" c !var p
    | _, false -> Printf.printf "(%d%c^%d)" c !var p ;;

let rec print_polyExpr (e : polyExpr): unit =
  match e with
  | Term (c, p) -> print_term c p
  | Plus list -> print_polyExpr_list e list "+"
  | Times list -> print_polyExpr_list e list "*"

and print_polyExpr_list (expr : polyExpr) (list : polyExpr list) (op : string) : unit =
  match list with
  | [] -> print_newline ()
  | hd :: [] -> print_polyExpr hd
  | hd :: tl -> (
    print_polyExpr hd;
    Printf.printf " %s " op;
    match expr with
    | Plus _ -> print_polyExpr (Plus tl)
    | Times _ -> print_polyExpr (Times tl)
    | _ -> ()) ;;

(* Computes degree of a polynomial expression *)
let rec degree (expr : polyExpr) : int =
  match expr with
  | Term (_, p) -> p
  | Plus list | Times list -> get_list_degree expr list

and get_list_degree (expr: polyExpr) (list : polyExpr list) : int =
  match list with
  | [] -> 0
  | hd :: [] -> degree hd
  | hd :: tl ->
    match expr with
    | Plus _ -> max (degree hd) (degree (Plus tl))
    | Times _ -> degree hd + degree (Times tl)
    | _ -> 0 ;;

let compare_degree (e1 : polyExpr) (e2 : polyExpr) : int =
  - compare (degree e1) (degree e2) ;;

let rec flatten_list (expr : polyExpr) : polyExpr list =
  match expr with
  | Term _ -> [expr]
  | Plus list -> (
    match list with
    | [] -> []
    | hd :: tl ->
      match hd with
      | Term _ -> hd :: flatten_list (Plus tl)
      | Plus list -> list @ flatten_list (Plus tl)
      | Times list -> Times (flatten_list hd) :: flatten_list (Plus tl))
  | Times list -> (
    match list with
    | [] -> []
    | hd :: tl ->
      match hd with
      | Term _ -> hd :: flatten_list (Times tl)
      | Plus list -> flatten_list hd @ flatten_list (Times tl)
      | Times list -> list @ flatten_list (Times tl)) ;;

let rec multiply (list : polyExpr list) : polyExpr = 
  match list with
  | [] -> Term (0, 0)
  | hd :: [] -> hd
  | hd1 :: hd2 :: tl ->
    match hd1 with
    | Term (a, b) -> (
      match hd2 with
      | Term (c, d) -> multiply (Term (a * c, b + d) :: tl)
      | Plus list -> multiply (Plus (List.map (fun expr -> multiply [hd1; expr]) list) :: tl)
      | Times list -> multiply (multiply (hd1 :: [multiply list]) :: tl))
    | Plus expr_list -> (
      match hd2 with
      | Term (c, d) -> multiply (Plus (List.map (fun expr -> multiply [hd2; expr]) expr_list) :: tl)
      | Plus list -> multiply (Plus (List.map (fun expr -> multiply [hd1; expr]) list) :: tl)
      | Times list -> multiply (multiply (hd1 :: [multiply list]) :: tl))
    | Times expr_list -> (
      match hd2 with
      | Term (c, d) -> multiply (multiply (hd2 :: [multiply expr_list]) :: tl)
      | Plus _ -> multiply (multiply (hd2 :: [multiply expr_list]) :: tl)
      | Times list -> multiply (multiply (list @ expr_list) :: tl))

let sort_list (list : polyExpr list) : polyExpr list = List.fast_sort compare_degree list ;;

let rec simplify_terms (expr : polyExpr) : polyExpr =
  match expr with
  | Term _ -> expr
  | Plus list -> (
    match list with
    | [] -> Term (0, 0)
    | hd :: [] -> hd
    | hd1 :: hd2 :: tl ->
      match hd1 with
      | Term (a, b) -> (
        match hd2 with
        | Term (c, d) ->
          (* print_polyExpr (); Printf.printf "\n"; *)
          if b = d then simplify_terms (Plus (Term (a + c, b) :: tl))
          else Plus (hd1 :: [simplify_terms (Plus (sort_list (hd2 :: tl)))])
        | Plus list -> Plus (hd1 :: [simplify_terms (Plus (sort_list (hd2 :: tl)))])
        | Times list -> Plus (sort_list (hd1 :: multiply list :: tl)))
      | Plus list -> simplify_terms (Plus (sort_list (hd2 :: list @ tl)))
      | Times list -> Plus (sort_list (multiply list :: hd2 :: tl)))
  | Times list -> multiply list ;;

let simplify_list (expr : polyExpr) : polyExpr list = 
  match expr with
  | Term _ -> [expr]
  | _ -> expr
    |> simplify_terms
    |> flatten_list
    |> sort_list ;;

let simplify_polyExpr (expr : polyExpr) : polyExpr =
  match expr with
  | Term (c, p) -> Term (c, p)
  | Plus list -> let simplified_list = simplify_list expr in
    let length = List.length simplified_list in
      (* print_polyExpr (simplify_terms (Plus simplified_list)); print_newline (); *)
      if length > 1 then Plus simplified_list
      else if length = 1 then List.hd simplified_list
      else Term (0, 0)
  | Times list -> let simplified_list = simplify_list (multiply list) in
    let length = List.length simplified_list in
      if length > 1 then Plus simplified_list
      else if length = 1 then List.hd simplified_list
      else Term (0, 0) ;;

let compare_poly_list (list1 : polyExpr list) (list2 : polyExpr list) : bool =
  if list1 = list2 then true
  else false ;;

let rec equal_polyExpr (e1 : polyExpr) (e2 : polyExpr) : bool =
  match e1, e2 with
  | Term (i1, i2), Term (i3, i4) -> (
    if i1 = i3 then
      if i2 = i4 then true
      else false
    else false)
  | Plus list1, Plus list2 -> compare_poly_list list1 list2
  | Times list1, Times list2 -> compare_poly_list list1 list2
  | _ -> false ;;

(*  Apply simplify_polyExpr until no progress is made *)    
let rec simplify (expr : polyExpr) : polyExpr =
  print_polyExpr expr;
  print_newline ();
  let simplifiedExpr : polyExpr = simplify_polyExpr expr in
    if equal_polyExpr expr simplifiedExpr then (print_newline (); expr) else simplify simplifiedExpr ;;