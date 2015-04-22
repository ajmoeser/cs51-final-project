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
    type clause = Empty | Disj of var * clause

    let rec clause_sat (c : clause) : bool =
      match c with
      | Empty -> false
      | Disj (h,t) ->
        if Vars.get_value h then true else clause_sat t

    let is_single (c : clause) : bool =
      match c with
      | Disj (v,Empty) -> true
      | _ -> false

(* NOT YET IMPLEMENTED

    let is_unit (c : clause) : bool =
      true

    let resolve (c1: clause) (c2 : clause) : clause =
      c1

    let unit_rule (c : clause) : clause =
      c

    let elim_mult_vars (c : clause) : clause =
      c

    let elim_taut (c : clause) : clause =
      c    
*)
end
