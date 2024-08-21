open Types


type expr =
  | TmBool   of bool (* Boolean term *)
  | TmNum    of float (* Numeric term *)
  | TmChoice of gprob * expr * expr (* Probabilistic Binary Choice *)
  | TmAscr   of expr * gdtype (* Ascription *)

