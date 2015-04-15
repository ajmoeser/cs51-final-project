(* This will be the interface and implementation of clauses *)
open Vars2 

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
 

    type var = Vars2.var  
    type clause = Empty | Cons of var * clause

    let rec clause_sat (c : clause) : bool =
      match c with
      | Empty -> true
      | Cons (h,t) ->
        if Vars2.get_value h then true else clause_sat t

    let is_unit (c : clause) : bool =
      true

    let resolve (c1: clause) (c2 : clause) : clause =
      c1

    let unit_rule (c : clause) : clause =
      c

    let is_single (c : clause) : bool =
      match c with
      | Cons (v,Empty) -> true
      | _ -> false

    let elim_mult_vars (c : clause) : clause =
      c

    let elim_taut (c : clause) : clause =
      c    

end
