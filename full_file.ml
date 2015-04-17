type var_name = Pos of bytes | Neg of bytes

type var = Unassn of var_name | Assn of var_name * bool
type clause = EmptyC | Conj of var * clause
type formula = EmptyF | Cons of clause * formula

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
  | _ -> v (* Haven't decided yet how to handle if already assigned*)

(*generates false positives*)
let rec clause_sat (c : clause) : bool =
  match c with
  | EmptyC -> false
  | Conj (h,t) ->
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
  | Conj (v,EmptyC) -> true
  | _ -> false

let elim_mult_vars (c : clause) : clause =
  c

let elim_taut (c : clause) : clause =
  c    

let empty () :  formula =
  EmptyF

let is_empty (f : formula) : bool =
  match f with 
  | EmptyF -> true
  | _ -> false

let has_empty (f : formula) : bool =
  true

let prelim_process (f : formula) : formula =
  f

let apply_unit (f : formula) : formula =
  f

let apply_resolution (f : formula) : formula =
  f
