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
let v4 = Assn (Neg (B),true)
let v5 = Assn (Pos (B),true)
let v6 = Assn (Neg (C),false)

let c1 = Disj (v1,EmptyC)
let c2 = Disj (v1,Disj (v2,EmptyC))
let c3 = Disj (v3,c2)
let c4 = Disj (v3,Disj (v4,EmptyC))

(* Checks whether variable has been assigned --- may not
 * be necessary --- can return option in get_value *)
let is_assigned (v : var) : bool =
  match v with
  | Unassn _ -> false
  | _ -> true

(* Returns value of variable, or None if unassigned *)
let get_value (v : var) : bool option =
  match v with
  | Assn (_,true) -> Some true
  | Assn (_,false) -> Some false
  | Unassn _ -> None
    
(* Assigns boolean value to variable*)
let assign_var (b : bool) (v : var) : var =
  match v with
  | Unassn x -> Assn (x,b)
  | _ -> v (* Haven't decided yet how to handle if already assigned*)

(* If any variable is assigned to be true, clause is satisfied *)
let rec clause_sat (c : clause) : bool =
  match c with
  | EmptyC -> false
  | Disj (h,t) ->
    (match h with
    | Assn (_,true) -> true
    | _ -> clause_sat t)

(* Tests whether clause is unit clause *)
let rec is_unit (c : clause) : bool =
  let rec rest_assigned cl =
    match cl with
    | EmptyC -> true
    | Disj (Assn (_,_),t) -> rest_assigned t
    | Disj (Unassn (_),_) -> false in
  match c with
  | EmptyC -> false
  | Disj (Unassn (_),EmptyC) -> true
  | Disj (Unassn (_),t) -> rest_assigned t
  | Disj (Assn (_,_),t) -> is_unit t

let resolve (c1: clause) (c2 : clause) : clause =
  c1

let unit_rule (c : clause) : clause =
  c

(* Tests whether clause is single variable; function used because
 * if clause is single, then that variable can only have one setting.*)
let is_single (c : clause) : bool =
  match c with
  | Disj (v,EmptyC) -> true
  | _ -> false

(* Simplifies clauses by eliminating mutiple instances
 * of the same variable *)
let elim_mult_vars (c : clause) : clause =
  let rec check_vars cl_init cl_result var_list =
    match cl_init with
    | EmptyC -> cl_result
    | Disj (Assn (x,b),tl)  -> 
      if List.mem x var_list then check_vars tl cl_result var_list
      else check_vars tl (Disj (Assn (x,b),cl_result)) (x::var_list)
    | Disj (Unassn x,tl) -> 
      if List.mem x var_list then check_vars tl cl_result var_list
      else check_vars tl (Disj (Unassn x,cl_result)) (x::var_list) in
  check_vars c EmptyC [] 

(* Eliminates clauses containing tautologies NEEDS TO BE FIXED*)
let rec elim_taut (c : clause) : bool = 
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
  | Disj (Unassn x,tl) -> elim_taut tl
 
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
let prelim_process (f : formula) : formula =
  let rec rebuild_form init result =
    match init with
    | EmptyF -> result
    | Conj (h,t) -> 
      if elim_taut h then rebuild_form t result
      else rebuild_form t (Conj (elim_mult_vars h,result))  in
  rebuild_form f EmptyF

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
