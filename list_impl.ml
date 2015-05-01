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
type clause = var list
type formula = clause list

(* variables and clauses for testing in Utop: *)
let v1 = Assn (Pos (A),true)
let v2 = Unassn (Pos (B))
let v3 = Unassn (Neg (B))
let v4 = Assn (Neg (B),false)
let v5 = Assn (Pos (B),true)
let v6 = Assn (Neg (C),false)

let c1 : clause = [v1]
let c2 : clause = [v1;v2]
let c3 : clause = [v3;v2;v1]
let c4 : clause = [v3;v4]
(* tautologies *)
let c5 : clause = [v2;v3] 
let c6 : clause = [v2;v4;v1;v6;v5]

(* Helper to extract var_name from var *)
let get_var_name (v : var) : var_name =
  match v with
  | Assn (x,_) | Unassn x -> x

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
  | [] -> false
  | h::t ->
    (match h with
    | Assn (_,true) -> true
    | _ -> clause_sat t)

let _ = assert ((clause_sat []) = false);;
let _ = assert ((clause_sat c1) = true);;
let _ = assert ((clause_sat c2) = true);;
let _ = assert ((clause_sat c4) = false);;

(* Tests whether clause is unit clause *)
let rec is_unit (c : clause) : bool =
  let rec rest_assigned_false cl =
    match cl with
    | [] -> true
    | (Assn (_,false)) :: t -> rest_assigned_false t
    | (Unassn (_)) :: _ | (Assn (_,true)) :: _ -> false in
  match c with
  | [] -> false
  | (Unassn (_)) :: [] -> true
  | (Unassn (_)) :: t -> rest_assigned_false t
  | (Assn (_,true)) :: _ -> false
  | (Assn (_,_)) :: t -> is_unit t

let _ = assert ((is_unit []) = false);;
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
  | _ :: [] -> true
  | _ -> false

let _ = assert ((is_single c1) = true);;
let _ = assert ((is_single c2) = false);;
let _ = assert ((is_single []) = false);;

(* Simplifies clauses by eliminating mutiple instances
 * of the same variable *)
let elim_mult_vars (c : clause) : clause =
  let rec check_vars cl_init cl_result var_list =
    match cl_init with
    | [] -> cl_result
    | ((Assn (x,b)) as hd) :: tl ->
      if List.mem var_list x then check_vars tl cl_result var_list
      else check_vars tl (hd :: cl_result) (x :: var_list)
    | ((Unassn x) as hd) :: tl ->
      if List.mem var_list x then check_vars tl cl_result var_list
      else check_vars tl (hd :: cl_result) (x :: var_list) in
  List.rev (check_vars c [] [])

(* clauses for testing elim_mult_vars *)
let mult_test1 = [v1;v1];;
let mult_test2 = [v1;v2;v1];;
let mult_test3 = [v1;v2;v3;v2];;

let _ = assert ((elim_mult_vars mult_test1) = [v1]);;
let _ = assert ((elim_mult_vars mult_test2) = [v1;v2]);;
let _ = assert ((elim_mult_vars mult_test3) = [v1;v2;v3]);;
let _ = assert ((elim_mult_vars []) = []);;

(* Identifies clauses containing tautologies so that they can be deleted *)
let rec elim_taut (c : clause) : bool =
  let check_oppo (v : var) (cl : clause) : bool =
    let n = get_var_name v in
    match n with
    | Pos x ->
      List.fold_left ~init:false cl 
         ~f:(fun _ z -> (get_var_name z) = Neg x)
    | Neg x ->
      List.fold_left ~init:false cl 
         ~f:(fun _ z -> (get_var_name z) = Pos x) in
  match c with
  | [] -> false
  | h::t -> (check_oppo h t) || elim_taut t 

let _ = assert ((elim_taut c1) = false);;
let _ = assert ((elim_taut c2) = false);;
let _ = assert ((elim_taut c5) = true);;
let _ = assert ((elim_taut c6) = true);;
let _ = assert ((elim_taut []) = false);;

(* Creates empty formula---May not be necessary*)
let empty () :  formula = []

(* Tests whether formula is empty *)
let is_empty (f : formula) : bool =
  match f with 
  | [] -> true
  | _ -> false

let _ = assert ((is_empty []) = true);;
let _ = assert ((is_empty [c1;c2]) = false);;


(* If the formula contains an empty clause, it is unsatisfiable. *)
let rec has_empty (f : formula) : bool =
  match f with
  | [] -> false
  | [] :: _ -> true
  | _ :: tl -> has_empty tl

let _ = assert ((has_empty [c1;[];c2]) = true);;
let _ = assert ((has_empty [c1;c2;[]]) = true);;
let _ = assert ((has_empty [c1;c2]) = false);;


(* Simplifies formula by eliminating tautologies and multiple
 * variables within same clause *)
let prelim_process (f : formula) : formula =
  let rec rebuild_formula init result =
    match init with
    | [] -> result
    | h :: t -> 
      if elim_taut h then rebuild_formula t result
      else rebuild_formula t ((elim_mult_vars h) :: result)  in
  rebuild_formula f []
  
let _ = assert ((prelim_process []) = []);;
let _ = assert ((prelim_process [c6;[v1;v2;v1]]) = [[v1;v2]]);;
let _ = assert ((prelim_process [c5;c6]) = []);;

(* Generates list of all variables in formula *)
let list_vars (f : formula) : var_name list =
  let rec list_clause_vars c acc =
    match c with
    | [] -> List.rev acc
    | hd :: tl -> list_clause_vars tl ((get_var_name hd) :: acc) in
  let rec list_form_vars (form : formula) (acc : var_name list) : 
                           var_name list =
    match form with
    | [] -> acc
    | hd :: tl -> list_form_vars tl (acc @ (list_clause_vars hd [])) in
  list_form_vars f []

let _ = assert ((list_vars []) = []);;
let _ = assert ((list_vars [c1]) = [Pos A]);;
let _ = assert ((list_vars [c5]) = [Pos B;Neg B]);;
let _ = assert ((list_vars [c5;c1]) = [Pos B;Neg B;Pos A]);;
let _ = assert ((list_vars [c5;c1;c5;[]]) = [Pos B;Neg B;Pos A;Pos B;Neg B]);;

let apply_unit (f : formula) : formula =
  f

let apply_resolution (f : formula) : formula =
   f

