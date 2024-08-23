open Lib.Matrix
open Lib.Solver
open Lib.Types
open Lib.Parser
open Lib.Typechecker
open Printf


(***********************************)
(* GRADUAL DISTRIBUTION TYPES DEMO *)
(***********************************)

  (* Gradual (simple) types     : GTReal , GTReal , GTDynamic *)
  (* Gradual distribution types : GDType( gtype list * gprob list ) *)


(* let gdt1 = GDType([GTReal ; GTReal], *)
(*                   [(GProb 0.2); (GProb 0.8)]) *)

(* let gdt2 = GDType([GTReal ; GTReal], *)
(*                   [(GProb 0.5); (GProb 0.5)]) *)

(* let gplst1 = List.map pp_gprob (get_gdt_prob_list gdt1) *)
(* let gplst2 = List.map pp_gprob (get_gdt_prob_list gdt2) *)
(* let cmatrix = consistency_matrix gdt1 gdt2 *)
(* let consistent = solve_coupling gplst1 gplst2 cmatrix *)
(* let () = printf "\nConsistent: %s\n" (Bool.to_string consistent) *)


(******************************)
(* PROGRAM TYPE CHECKING DEMO *)
(******************************)

let program_name : string = Sys.argv.(1)
let path : string  = sprintf "./programs/%s.gplc" program_name
let s : string = pp_gdtype (typecheck ( parse (sexp_from_file path)))
let () = printf "\n> %s\n" s
