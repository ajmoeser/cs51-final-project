(* Alexander Moeser
 * CS51 Final Project : DPLL SAT-Solver
 * 
 * This file contains the main algorithm for a SAT-solver using the
 * Davis-Putnam-Logemann-Loveland ("DPLL") algorithm.  Other associated
 * files contain interfaces and implementations for variables, clauses,
 * and formulas.  See README.md for details.
 *
*)

open Core.Std
open List_impl

exception UNSATISFIABLE


   
(* core recursive function to determine whether a formula is satisfiable
 * given a particular mapping of variables *)
let rec sat_search (f : formula) (m : var_map) : var_map =
  (* Two preliminary steps to simplify formula and
   * apply pure literal rule *)
  let f' = prelim_process f in
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



(* To be implemented : function to add one more variable mapping
 * to the var_map when sat_search is called recursively.

let add_next_variable (f : formula) (p : var_map) : var_map =
  TODO

*)  
