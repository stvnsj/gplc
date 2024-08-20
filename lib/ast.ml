open Types


type expr =
  | TmBool   of bool
  | TmNum    of float
  | TmChoice of gprob * expr * expr
  | TmAscr   of expr * gdtype

