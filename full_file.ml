open Core.Std

(* Limiting variables for testing*)
type var_literal = | A
		   | B
		   | C
		   | D
		   | E

(* Representing "x" and "not x" *)
type var_name = Pos of var_literal | Neg of var_literal

(* The three main datatypes ; in the final version, these will
 * be in separate modules *)
type var = Unassn of var_name | Assn of var_name * bool
type clause = EmptyC | Disj of var * clause
type formula = EmptyF | Conj of clause * formula

(* variables and clauses for testing in Utop: *)
let v1 = Assn (Pos (A),true)
let v2 = Unassn (Pos (B))
let v3 = Unassn (Neg (B))
let v4 = Assn (Neg (B),false)
let v5 = Assn (Pos (B),true)
let v6 = Assn (Neg (C),false)

let c1 = Disj (v1,EmptyC)
let c2 = Disj (v1,Disj (v2,EmptyC))
let c3 = Disj (v3,c2)
let c4 = Disj (v3,Disj (v4,EmptyC))
let c5 = Disj (v2,Disj (v3,EmptyC)) (* tautology *)

(* Checks whether variable has been assigned --- may not
 * be necessary --- can return option in get_value *)
let is_assigned (v : var) : bool =
  match v with
  | Unassn _ -> false
  | _ -> true

let _ = assert ((is_assigned v1) = true);;
let _ = assert ((is_assigned v2) = false);;

(* Returns value of variable, or None if unassigned *)
let get_value (v : var) : bool option =
  match v with
  | Assn (_,true) -> Some true
  | Assn (_,false) -> Some false
  | Unassn _ -> None
    
let _ =  assert ((get_value v1) = Some true);;
let _ =  assert ((get_value v2) = None);;

(* Assigns boolean value to variable*)
let assign_var (b : bool) (v : var) : var =
  match v with
  | Unassn x -> Assn (x,b)
  | _ -> v

let _ = assert ((assign_var false v1) = Assn (Pos A,true));;
let _ = assert ((assign_var false v2) = Assn (Pos B,false));;

(* If any variable is assigned to be true, clause is satisfied *)
let rec clause_sat (c : clause) : bool =
  match c with
  | EmptyC -> false
  | Disj (h,t) ->
    (match h with
    | Assn (_,true) -> true
    | _ -> clause_sat t)

let _ = assert ((clause_sat EmptyC) = false);;
let _ = assert ((clause_sat c1) = true);;
let _ = assert ((clause_sat c2) = true);;
let _ = assert ((clause_sat c4) = false);;

(* Tests whether clause is unit clause *)
let rec is_unit (c : clause) : bool =
  let rec rest_assigned_false cl =
    match cl with
    | EmptyC -> true
    | Disj (Assn (_,false),t) -> rest_assigned_false t
    | Disj (Unassn (_),_) | Disj (Assn (_,true),_) -> false in
  match c with
  | EmptyC -> false
  | Disj (Unassn (_),EmptyC) -> true
  | Disj (Unassn (_),t) -> rest_assigned_false t
  | Disj (Assn (_,true),_) -> false
  | Disj (Assn (_,_),t) -> is_unit t

let _ = assert ((is_unit EmptyC) = false);;
let _ = assert ((is_unit c1) = false);;
let _ = assert ((is_unit c2) = false);;
let _ = assert ((is_unit c3) = false);;
let _ = assert ((is_unit c4) = true);;

let resolve (c1: clause) (c2 : clause) : clause =
  c1

let unit_rule (c : clause) : clause =
  c

(* Tests whether clause is single variable; function used because
 * if clause is single, then that variable can only have one setting.*)
let is_single (c : clause) : bool =
  match c with
  | Disj (_,EmptyC) -> true
  | _ -> false

let _ = assert ((is_single c1) = true);;
let _ = assert ((is_single c2) = false);;
let _ = assert ((is_single EmptyC) = false);;

(* Simplifies clauses by eliminating mutiple instances
 * of the same variable *)
let elim_mult_vars (c : clause) : clause =
  let rec check_vars cl_init cl_result var_list =
    match cl_init with
    | EmptyC -> cl_result
    | Disj (Assn (x,b),tl)  -> 
      if List.mem var_list x then check_vars tl cl_result var_list
      else check_vars tl (Disj (Assn (x,b),cl_result)) (x::var_list)
    | Disj (Unassn x,tl) -> 
      if List.mem var_list x then check_vars tl cl_result var_list
      else check_vars tl (Disj (Unassn x,cl_result)) (x::var_list) in
  check_vars c EmptyC [] 

let mult_test1 = Disj (v1,Disj(v1,EmptyC));;
let mult_test2 = Disj (v1,Disj(v2,Disj (v1,EmptyC)));;
let mult_test3 = Disj (v1,Disj(v2,Disj (v2,EmptyC)));;

(* NOTE: elim_mult_vars correctly removes repeated variables but
 * reverses clause order *)
let _ = assert ((elim_mult_vars mult_test1) = Disj (v1,EmptyC));;
let _ = assert ((elim_mult_vars mult_test2) = Disj (v2,Disj (v1,EmptyC)));;
let _ = assert ((elim_mult_vars mult_test3) = Disj (v2,Disj (v1,EmptyC)));;

(* Eliminates clauses containing tautologies NEEDS TO BE FIXED*)
(*let rec elim_taut (c : clause) : bool = 
  let rec elim_neg v cl =
    match cl with
    | EmptyC -> false
    | Disj (Assn (Neg v,_),tl) -> true
    | Disj (_,tl) -> elim_neg v tl in
  let rec elim_pos v cl =
    match cl with
    | EmptyC -> false
    | Disj (Assn (Pos v,_),tl) -> true
    | Disj (_,tl) -> elim_pos v tl in
  match c with
  | EmptyC -> false
  | Disj (Assn (Pos x,_),tl) -> elim_neg x tl
  | Disj (Assn (Neg x,_),tl) -> elim_pos x tl
  | Disj (Unassn (Pos x),tl) -> elim_neg x tl
  | Disj (Unassn (Neg x),tl) -> elim_pos x tl
*)

(* NOT WORKING*)
let elim_taut (c : clause) : clause =
  let rec elim_neg v cl =
    match cl with
    | EmptyC -> cl
    | Disj (Assn (Pos v,_),t) | Disj (Unassn (Pos v),t) -> elim_neg v t
    | Disj (h,t) -> Disj (h,(elim_neg v t)) in
  let rec elim_pos v cl =
    match cl with
    | EmptyC -> cl
    | Disj (Assn (Neg v,_),t) | Disj (Unassn (Neg v),t) -> elim_pos v t
    | Disj (h,t) -> Disj (h,(elim_pos v t)) in
  let rec elim (init : clause) (result : clause) : clause =
    match init with
    | EmptyC -> result
    | Disj ((Assn (Pos x,_)) as hd,tl) ->
      elim (elim_neg x tl) (Disj (hd, result))
    | Disj ((Unassn (Pos x)) as hd,tl) ->
      elim (elim_neg x tl) (Disj (hd, result))
    | Disj ((Assn (Neg x,_)) as hd,tl) ->
      elim (elim_pos x tl) (Disj (hd, result))
    | Disj ((Unassn (Neg x)) as hd,tl) ->
      elim (elim_pos x tl) (Disj (hd, result)) in
  elim c EmptyC


let _ = assert ((elim_taut c1) = c1);;
let _ = assert ((elim_taut c2) = c2);;
let _ = assert ((elim_taut c5) = EmptyC);;
let _ = assert ((elim_taut EmptyC) = EmptyC);;
 
(* Creates empty formula---May not be necessary*)
let empty () :  formula =
  EmptyF

(* Tests whether formula is empty *)
let is_empty (f : formula) : bool =
  match f with 
  | EmptyF -> true
  | _ -> false

(* If the formula contains an empty clause, it is unsatisfiable. 
 * Note: if last clause is only empty clause, will return true,
 * since every formula ends with empty clause. *)
let rec has_empty (f : formula) : bool =
  match f with
  | EmptyF -> false
  | Conj (EmptyC,_) -> true
  | Conj (_,tl) -> has_empty tl

(* Simplifies formula by eliminating tautologies and multiple
 * variables within same clause *)
(*let prelim_process (f : formula) : formula =
  let rec rebuild_form init result =
    match init with
    | EmptyF -> result
    | Conj (h,t) -> 
      if elim_taut h then rebuild_form t result
      else rebuild_form t (Conj (elim_mult_vars h,result))  in
  rebuild_form f EmptyF
  *)
let list_vars (f : formula) : var_name list =
  let rec list_clause_vars c acc =
    match c with
    | EmptyC -> acc
    | Disj (Assn ( x,_),tl) | Disj (Unassn (x),tl) ->
      list_clause_vars tl (x::acc) in
  let rec list_form_vars (form : formula) (acc : var_name list) : 
                           var_name list =
    match f with
    | EmptyF -> acc
    | Conj (h,tl) -> list_form_vars tl (list_clause_vars h acc) in
  list_form_vars f []

let apply_unit (f : formula) : formula =
  f

let apply_resolution (f : formula) : formula =
   f
