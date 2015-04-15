type value = bool
type name = bytes

type var = Unassn of name | Assn of name * value
type clause = Empty | Cons of var * clause
type formula = Empty | Cons of clause * formula

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

(*false positive*)
let rec clause_sat (c : clause) : bool =
  match c with
  | Empty -> true
  | Cons (h,t) ->
    (match h with
    | Assn (_,true) -> true
    | _ -> clause_sat t)

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

let empty () :  formula =
  Empty

let is_empty (f : formula) : bool =
  match f with 
  | Empty -> true
  | _ -> false

let prelim_process (f : formula) : formula =
  f

let apply_unit (f : formula) : formula =
  f


let apply_resolution (f : formula) : formula =
  f
