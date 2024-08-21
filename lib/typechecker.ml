

open Types
open Ast
open Printf

let rec typecheck : expr -> gdtype =
  fun e -> 
  match e with

  (* Boolean value returns type {{Bool^1.0}} *)
  | TmBool _ -> GDType ([GTBool],[(GProb 1.0)])

  (* Real value returns type {{Real^1.0}} *)
  | TmNum  _ -> GDType ([GTReal],[(GProb 1.0)])

  | TmChoice (gp, e1, e2) ->
     let gdt1 = typecheck e1 in (* t1 *)
     let gdt2 = typecheck e2 in (* t2 *)
     let comp = gprob_comp gp in (* Complement of gradual probability gp *)
     let scale1 = scale_gdtype gp gdt1 in (* p . t1 *)
     let scale2 = scale_gdtype comp gdt2 in (* (1-p) . t2 *)
     add_gdtype scale1 scale2 (* p . t1 + (1-p) . t2 *)

  | TmAscr (term,typ) ->
     let typ0 = typecheck term in (* Type of term *)
     let consistent = gdtype_consistency typ0 typ in 
     if consistent then typ0
     else failwith "Inconsistent types"


  | _ -> failwith (sprintf "Type Error!")
