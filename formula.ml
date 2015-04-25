(* This will be the implementation and interface of formulas *)
open Clause

module type FORMULA =
  sig

    type formula

    val empty : unit -> formula
    val is_empty : formula -> bool
    val has_empty : formula -> bool
    val prelim_process : formula -> formula
    val apply_unit : formula -> formula
    val apply_resolution : formula -> formula

  end

module Form : FORMULA =
  struct
  
  type formula = EmptyF | Conj of clause * formula

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

    let apply_unit (f : formula) : formula =
      f

    let apply_resolution (f : formula) : formula =
      f


end
