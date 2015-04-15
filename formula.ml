(* This will be the implementation and interface of formulas *)


module type FORMULA =
  sig

    type formula

    val empty : unit -> formula
    val is_empty : formula -> bool
    val prelim_process : formula -> formula
    val apply_unit : formula -> formula
    val apply_resolution : formula -> formula

  end
