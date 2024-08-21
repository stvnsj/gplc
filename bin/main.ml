open Lib.Matrix
open Lib.Solver
open Lib.Types
open Lib.Parser
open Lib.Typechecker
open Printf

(******************************)
(* PROGRAM TYPE CHECKING DEMO *)
(******************************)

let program_name : string = Sys.argv.(1)
let path : string  = sprintf "./programs/%s.gplc" program_name
let s : string = pp_gdtype (typecheck ( parse (sexp_from_file path)))
let () = printf "> %s\n" s
