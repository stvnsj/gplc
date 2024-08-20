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
               [GTReal ; GTBool ; GTBool],
               [(GProb 0.2) ; (GProb 0.2) ; GProbDynamic])

let gdt2 = GDType (
               [GTReal ; GTBool],
               [GProbDynamic ; GProbDynamic])


let lst1 = List.map pp_gprob (get_gdt_prob_list gdt1)
let lst2 = List.map pp_gprob (get_gdt_prob_list gdt2)


let m = (solve_coupling
           lst1
           lst2
           (consistency_matrix gdt1 gdt2))




(******************************)
(* PROGRAM TYPE CHECKING DEMO *)
(******************************)
let parsed_sexp  = sexp_from_string "true"
let program_ast  = parse parsed_sexp
let program_type = typecheck program_ast
let type_string  = pp_gdtype program_type

let gp1 = GProb 0.3
let gp2 = GProb 0.2
let gp3 = GProbDynamic

let gprob_operation1 = op2_gprob ( +. ) gp1 gp2
let gprob_operation2 = op2_gprob ( *. ) gp1 gp2



let () = printf "\n\n%s\n\n" type_string
