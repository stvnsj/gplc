

open Types
open Ast
open Printf

let rec typecheck : expr -> gdtype =
  fun e -> 
  match e with
  | TmBool _ -> GDType ([GTBool],[(GProb 1.0)])
  | _ -> failwith (sprintf "Type Error!")
