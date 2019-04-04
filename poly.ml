(* Sum type to encode efficiently polynomial expressions
  Term:   First int is the constant
          Second int is the power of x 
          10  -> Term(10,0)
          2x -> Term(2,1)
          3x^20 -> Term(3, 20) 
  Plus:   List of terms added
          Plus([Term(2,1); Term(1,0)])
  Times:  List of terms multiplied
          Times([Term(2,1); Term(1,0)]) *)
type polyExpr =
  | Term    of int * int 
  | Plus    of polyExpr list
  | Minus   of polyExpr list
  | Times   of polyExpr list
  | Divide  of polyExpr list
  | Negate  of polyExpr ;;

(* Function to traslate betwen AST expressions
  to polyExpr expressions *)
let rec from_expr (e : Expr.expr) : polyExpr =
  match e with
  | Num i -> Term (i, 0)
  | Var v -> Term (1, 1)
  | Add (e1, e2) -> Plus [from_expr e1; from_expr e2]
  | Sub (e1, e2) -> Minus [from_expr e1; from_expr e2]
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

(*  Compute degree of a polynomial expression.
    Hint 1: Degree of Term(n,m) is m
    Hint 2: Degree of Plus[...] is the max of the degree of args
    Hint 3: Degree of Times[...] is the sum of the degree of args *)
let rec degree (expr : polyExpr) : int =
  match expr with
  | Term (_, p) -> p
  | Plus list -> get_list_degree expr list
  | Minus list -> get_list_degree expr list
  | Times list -> get_list_degree expr list
  | Divide list -> get_list_degree expr list
  | Negate poly_expr -> degree poly_expr

and get_list_degree (expr: polyExpr) (list : polyExpr list) : int =
  match list with
  | [] -> 0
  | hd :: [] -> degree hd
  | hd :: tl ->
    match expr with
    | Plus _ -> if degree hd > degree (Plus tl) then degree hd else degree (Plus tl)
    | Minus _ -> if degree hd > degree (Minus tl) then degree hd else degree (Minus tl)
    | Times _ -> if degree hd > degree (Times tl) then degree hd else degree (Times tl)
    | Divide _ -> if degree hd > degree (Divide tl)then degree hd else degree (Divide tl)
    | _ -> 0 ;;

(*  Comparison function useful for sorting of Plus[..] args 
    to "normalize them". This way, terms that need to be reduced
    show up one after another *)
let compare (e1 : polyExpr) (e2 : polyExpr) : bool =
  degree e1 > degree e2 ;;

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
    | 1 -> Printf.printf "x"
    | _ -> Printf.printf "x%d" p)
  | _ -> (
    match p with
    | 0 -> Printf.printf "%d" c
    | 1 -> Printf.printf "%dx" c
    | _ -> Printf.printf "%dx^%d" c p) ;;


let rec print_polyExpr (e : polyExpr): unit =
  match e with
  | Term (c, p) -> print_term c p
  | Plus list -> print_polyExpr_list e list "+"
  | Minus list -> print_polyExpr_list e list "-"
  | Times list -> print_polyExpr_list e list "*"
  | Divide list -> print_polyExpr_list e list "/"
  | Negate expr -> Printf.printf "-"; print_polyExpr expr

and print_polyExpr_list (poly_expr : polyExpr) (poly_expr_list : polyExpr list) (op : string) : unit =
  match poly_expr_list with
  | [] -> print_newline ()
  | hd :: [] -> print_polyExpr hd
  | hd :: tl -> (
    print_polyExpr hd;
    Printf.printf "%s" op;
    match poly_expr with
    | Plus _ -> print_polyExpr (Plus tl)
    | Minus _ -> print_polyExpr (Minus tl)
    | Times _ -> print_polyExpr (Times tl)
    | Divide _ -> print_polyExpr (Divide tl)
    | _ -> ()) ;;

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
let simplify_polyExpr (e : polyExpr) : polyExpr =
  match e with
  | Term(_, _) -> Term(0, 1)
  | _ -> e
  ;;

(* Compute if two polyExpr are the same 
   Make sure this code works before you work on simplify1 *)
let compare_poly_list (list1 : polyExpr list) (list2 : polyExpr list) : bool =
  if list1 = list2 then true
  else false ;;

let rec equal_polyExpr (e1 : polyExpr) (e2 : polyExpr) : bool =
  match (e1, e2) with
  | (Term (i1, i2), Term (i3, i4)) -> (
    if i1 = i2 then
      if i3 = i4 then true
      else false
    else false)
  | (Plus list1, Plus list2) -> compare_poly_list list1 list2
  | (Minus list1, Minus list2) -> compare_poly_list list1 list2
  | (Times list1, Times list2) -> compare_poly_list list1 list2
  | (Divide list1, Divide list2) -> compare_poly_list list1 list2
  | (Negate expr1, Negate expr2) -> equal_polyExpr expr1 expr2
  | _ -> false ;;

(*  Fixed point version of simplify1 
    i.e. Apply simplify_polyExpr until no progress is made *)    
let rec simplify (e : polyExpr) : polyExpr =
  let rE = simplify_polyExpr e in
    print_polyExpr rE;
    print_newline ();
    if equal_polyExpr e rE then e else simplify rE ;;