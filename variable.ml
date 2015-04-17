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

    type var_name = Pos of bytes | Neg of bytes

    type var = Unassn of var_name | Assn of var_name * bool

    let is_assigned (v : var) : bool =
      match v with
      | Unassn _ -> false
      | _ -> true

    let get_value (v : var) : bool option =
      match v with
      | Assn (_,true) -> Some true
      | Assn (_,false) -> Some false
      | Unassn _ -> None
	

    let assign_var (b : bool) (v : var) : var =
      match v with
      | Unassn x -> Assn (x,b)
      | _ -> v (* Haven't decided yet how to handle if already assigned - may make this function return var option*)

end
