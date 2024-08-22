open Types


type expr =
  | TmBool   of bool (* Boolean term *)
  | TmNum    of float (* Numeric term *)
  | TmChoice of gprob * expr * expr (* Probabilistic Binary Choice *)
  | TmAscr1  of expr * gtype
  | TmAscr2  of expr * gdtype (* Ascription *)

