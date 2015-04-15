(* This will be the interface and implementation of variables *)

module type VARIABLE = 

sig

  type var
 
  val is_assigned : var -> bool
(*  val get_value : var -> bool*)
  val assign_var : bool -> var -> var

end

