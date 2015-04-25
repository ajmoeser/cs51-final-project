(* This will be the interface and implementation of variables *)

module type VARIABLE = 

sig

  type var
 
  val is_assigned : var -> bool
  val get_value : var -> bool option
  val assign_var : bool -> var -> var

end


module Vars : VARIABLE =
  struct

(* Limiting variables to simplify testing*)
    type var_literal = | A
		       | B
		       | C
		       | D
		       | E

(* Representing "x" and "not x" *)
    type var_name = Pos of var_literal | Neg of var_literal

    type var = Unassn of var_name | Assn of var_name * bool

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
      | _ -> v

end
