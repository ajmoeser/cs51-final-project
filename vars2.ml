module Vars2 : sig
 type var
 
  val is_assigned : var -> bool
  val get_value : var -> bool
  val assign_var : bool -> var -> var

end = struct


  type value = bool
  type name = bytes
  type var_val = Unassn of name | Assn of name * value

  type var = var_val

  let is_assigned (v : var) : bool =
    match v with
    | Unassn _ -> false
    | _ -> true

  let get_value (v : var) : bool =
    match v with
    | Assn (_,true) -> true
    | Assn (_,false) -> false
    | Unassn _ -> failwith "not assigned"

  let assign_var (b : bool) (v : var) : var =
    match v with
    | Unassn x -> Assn (x,b)
    | _ -> v


end
