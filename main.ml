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
 * given a particular set of variable assignments *)
let rec sat_search (f : formula) (vl : var list)  =
  let f' = prelim_process f in
  if formula_sat f' then vl
  else if has_empty f' then raise UNSATISFIABLE
  else let new_f = apply_unit f' in
   sat_search new_f (add_next_variable vl)
;;


(* Some example formulas. Note that unsatisfiable formulas
 * are commented out so that the program can run*)

let var1 = Unassn (Pos A)
let var2 = Unassn (Neg A)
let var3 = Unassn (Pos B)
let var4 = Unassn (Neg B)
let var5 = Unassn (Pos C)
let var6 = Unassn (Neg C)
let var7 = Unassn (Pos D)
let var8 = Unassn (Neg D)
let var9 = Unassn (Pos E)
let var10 = Unassn (Neg E)

let form1 : formula = [];;
let form2 = [[var1;var2]];;
let form3 = [[var1;var3];[var2;var3]];;
let form4 = [[var2];[var1]];;
let form5 = [[var2];[var3];[var5];[var8]];;
let form6 = [[var5;var6];[var7;var8];[var9;var10]];; 

(*Empty formula is automatically satisfiable*)
let _ = assert ((sat_search form1 (initial_vars (list_vars form1))) = initial_vars (list_vars form1));;
(*
(* (A or Not A) *)
let _ = assert ((sat_search form2 (initial_vars (list_vars form2))) = initial_vars (list_vars form1));;

(* (A or B) and (Not A or B)*)
let _ = assert ((sat_search form3 (initial_vars (list_vars form3))) = initial_vars (list_vars form3));;

(* (Not A) and (A)*)
(*let _ = assert ((sat_search form4 (initial_vars (list_vars form4))) = UNSATISFIABLE);;*)

(* testing a series of non-conflicting pure literals*)
let _ = assert ((sat_search form5 (initial_vars (list_vars form5))) = initial_vars (list_vars form3));;

(* testing a series of tautologies : returns all variables unassigned*)
let _ = assert ((sat_search form6 (initial_vars (list_vars form6))) = initial_vars (list_vars form6));;
*)
