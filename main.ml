(* Alexander Moeser
 * CS51 Final Project
 * 
 * This file contains the main algorithm for a SAT-solver using the
 * Davis-Putnam-Logemann-Loveland ("DPLL") algorithm.  Other associated
 * files contain interfaces and implementations for variables, clauses,
 * and formulas.  See README.md for details.
*)
open Core.Std

exception UNSATISFIABLE

(* I need to review how to use the Map module to create
 * a mapping of variable literals to assignments *) 
type var_map = Map.make (Variable.var_name) 

(* Gets a list if variable names to add to initial var_map *)
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

(* Gets initial variable map from formula; all values initially
 * unassigned *)
let rec get_var_map (f : formula) : var_map =
  let new = Map.empty in
  let vs = list_vars f in
  List.fold_right ~f:(fun x -> Map.add x (Unassn x)) ~init:new vs

   
(* core recursive function to determine whether a formula is satisfiable
 * given a particular mapping of variables *)
let rec sat_search (f : formula) (m : var_map) : var_map =
  (* Two preliminary steps to simplify formula and
   * apply pure literal rule *)
  let f' = Formula.prelim_process f in
  let m' = pure_literal m in
  let (f_new,map_new) = unit_propagate f' m' in
    match f_new with
    (* If, after propagation, the formula is empty, the assignments
     * in the current var_map satisfy the formula *)
    | EmptyF -> map_new
    | _ ->
      (* If under any assignment, a formula contains any empty clauses, 
       * the formula is unsatisfiable *)
      if has_empty f_new then raise UNSATISFIABLE
      (*otherwise, add a new variable to the mapping, and repeat*)
      else sat_search f_new (add_next_variable f_new map_new)

;;

(* To be implemented : the actual unit propagation function, which will
 * apply the unit rule to the formula, dropping satisfied clauses and
 * updating the var_map with assignments dictated by the unit rule

let rec unit_propagate (f : formula) (p : var_map) : formula * var_map =
  TODO *)
  
(* If "x" appears in the formula but "not x" does not, the var_map
 * must map x to true, and false if vice versa *)
let rec pure_literal (m : var_map) : var_map =
  let opposite_appears v_name v_map =
    match v_name with
    | Pos x -> Map.mem (Neg x) v_map
    | Neg x -> Map.mem (Pos x) v_map in
  match m with
  | EmptyF -> m
  | Conj (hd,tl) -> 
    if opposite_appears hd m then pure_literal tl m
    else match hd with
    | Pos x -> let new_m = Map.add x true m in pure_literal tl new_m
    | Neg x -> let new_m = Map.add x false m in pure_literal tl new_m


(* To be implemented : function to add one more variable mapping
 * to the var_map when sat_search is called recursively.

let add_next_variable (f : formula) (p : var_map) : var_map =
  TODO

*)  
