

open Types
open Ast
open Printf


(** Sub-typechecker in charge of gradual simple types ascriptions *)
let rec typecheck_gradual_type : expr -> gtype option =
  fun e ->
  match e with
  | TmBool _ -> Some GTBool
  | TmNum _ -> Some GTReal
  | TmAscr1 (term, typ) ->
     (match (typecheck_gradual_type term) with
      | Some t -> if (gtype_consistency t typ) then Some typ else failwith "ERROR"
      | None -> failwith "No a simple type")
  | _ -> None 



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

  | TmAscr1 (term,typ) ->
     (match (typecheck_gradual_type term) with
      | Some GTReal ->
         let consistent = gtype_consistency GTReal typ in
         if consistent then GDType([typ],[(GProb 1.0)])
         else failwith "Inconsistent Types"
         
      | Some GTBool ->
         let consistent = gtype_consistency GTBool typ in
         if consistent then GDType([typ],[(GProb 1.0)])
         else failwith "Inconsistent Types"
         
      | None -> failwith "Inconsistent types")
     
  | TmAscr2 (term,typ) ->
     let typ0 = typecheck term in (* Type of term *)
     let consistent = gdtype_consistency typ0 typ in 
     if consistent then typ
     else failwith "Inconsistent types"


  | _ -> failwith (sprintf "Type Error!")
