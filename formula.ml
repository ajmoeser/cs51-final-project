(* This will be the implementation and interface of formulas *)
Open Clauses

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

    type formula = EmptyF | Cons of clause * formula

    let empty () :  formula =
      EmptyF

    let is_empty (f : formula) : bool =
      match f with 
      | EmptyF -> true
      | _ -> false

(* TO BE IMPLEMENTED

    let has_empty (f : formula) : bool =
      true

    let prelim_process (f : formula) : formula =
      f

    let apply_unit (f : formula) : formula =
      f

    let apply_resolution (f : formula) : formula =
      f
*)
end
