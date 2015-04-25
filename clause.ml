(* This will be the interface and implementation of clauses *)

open Variable


module type CLAUSE =
  sig

    type clause

    val clause_sat : clause -> bool
    val is_unit : clause -> bool
    val resolve : clause -> clause -> clause
    val unit_rule : clause -> clause
    val is_single : clause -> bool
    val elim_mult_vars : clause -> clause
    val elim_taut : clause -> clause

  end

module Clauses : CLAUSE =
  struct
 
    type var = Vars.var  
    type clause = EmptyC | Disj of var * clause

	
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
	
end
