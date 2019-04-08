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
  | Var v -> var := v; Term (1, 1)
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
          | 0 -> [Term (1, 0)]
          | 1 -> poly_expr :: acc
          | _ -> pow_exp (pow - 1) (poly_expr :: acc)
        else [Term (0, 0)] in
      Times (pow_exp i [])) ;;

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

(*  Print a polyExprr nicely 
    Term(3,0) -> 3
    Term(5,1) -> 5x 
    Term(4,2) -> 4x^2
    Plus... -> () + () 
    Times ... -> ()() .. ()

    Hint 1: Print () around elements that are not Term() 
    Hint 2: Recurse on the elements of Plus[..] or Times[..] *)

let print_term (c : int) (p : int) : unit =
  match c with
  | 0 -> Printf.printf "0"
  | 1 -> (
    match p with
    | 0 -> Printf.printf "1"
    | 1 -> Printf.printf "%c" !var
    | _ -> Printf.printf "%c%d" !var p)
  | _ -> (
    match p with
    | 0 -> Printf.printf "%d" c
    | 1 -> Printf.printf "%d%c" c !var
    | _ -> Printf.printf "%d%c^%d" c !var p) ;;

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
    Printf.printf "%s" op;
    match expr with
    | Plus _ -> print_polyExpr (Plus tl)
    | Times _ -> print_polyExpr (Times tl)
    | _ -> ()) ;;

(*  Comparison function useful for sorting of Plus[..] args 
    to "normalize them". This way, terms that need to be reduced
    show up one after another *)
let compare_degree (e1 : polyExpr) (e2 : polyExpr) : int =
  let comparison = compare (degree e1) (degree e2) in
    if comparison <> 0 then -1 * comparison
    else comparison ;;

let rec flatten_list (expr : polyExpr) : polyExpr list =
  match expr with
  | Term _ -> [expr]
  | Plus list -> (
    match list with
    | [] -> []
    | hd :: tl ->
      match hd with
      | Term _ | Times _ -> hd :: flatten_list (Plus tl)
      | Plus list -> list @ flatten_list (Plus tl))
  | Times list -> (
    match list with
    | [] -> []
    | hd :: tl ->
      match hd with
      | Term _ | Plus _ -> hd :: flatten_list (Times tl)
      | Times list -> list @ flatten_list (Times tl)) ;;

let simplify_list (expr : polyExpr) : polyExpr list = 
  let sort_list (list : polyExpr list) : polyExpr list = List.fast_sort compare_degree list in
    match expr with
    | Term _ -> [expr]
    | Plus _ | Times _ -> expr
      |> flatten_list
      |> sort_list ;;

(*  Function to simplify (one pass) polyExprr

    n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
    Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

    Hint 1: Keep terms in Plus[...] sorted
    Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
    Hint 3: flatten times, i.e. times of times is times
    Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
            Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
    Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
      i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
        => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
        => Plus[Term(2,3); Term(6,5)]
    Hint 6: Find other situations that can arise *)
let simplify_polyExpr (expr : polyExpr) : polyExpr =
  match expr with
  | Term (c, p) -> Term (c, p)
  | Plus list -> Plus (simplify_list expr)
  | Times list -> Times (simplify_list expr) ;;

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
let rec simplify (e : polyExpr) : polyExpr =
  Printf.printf "degree: %d\n" (degree e);
  print_polyExpr e;
  print_newline ();
  let rE : polyExpr = simplify_polyExpr e in
    if equal_polyExpr e rE then e else simplify rE ;;