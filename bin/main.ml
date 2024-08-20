open Lib.Matrix
open Lib.Solver
open Lib.Types
open Lib.Parser
open Lib.Typechecker
open Printf


(* let bin_matrix = [[false;true;true]; *)
(*                   [true;false;true]; *)
(*                   [true;true;false]] *)

(* let () = (solve_coupling *)
(*             ["20.0" ; "10.0" ; "50.0"] *)
(*             ["28.0" ; "38.0" ; "14.0"] *)
(*             bin_matrix ) *)


let gdt1 = GDType (
               [GTReal ; GTReal ; GTBool ;GTBool ; GTDynamic],
               [(GProb 0.1) ; (GProb 0.1) ; (GProb 0.1) ; (GProb 0.1) ; (GProb 0.6)])

let gdt2 = GDType (
               [GTReal ; GTDynamic],
               [(GProb 0.1) ; GProbDynamic])


let lst1 = List.map pp_gprob (get_gdt_prob_list gdt1)
let lst2 = List.map pp_gprob (get_gdt_prob_list gdt2)


let m = (solve_coupling
           lst1
           lst2
           (consistency_matrix gdt1 gdt2))




(******************************)
(* PROGRAM TYPE CHECKING DEMO *)
(******************************)
let parsed_sexp  = sexp_from_string "(choice 0.9 (choice 0.3 1 7) true)"
let program_ast  = parse parsed_sexp
let program_type = typecheck program_ast
let type_string  = pp_gdtype program_type

let () = printf "\n\n%s\n\n" type_string
