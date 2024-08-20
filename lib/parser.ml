
open Printf
open CCSexp
open List
open Ast
open Types



(*
  Proceses program syntax and returns an AST
 *)
let rec parse (sexp : sexp) : expr =
  match sexp with
  | `Atom "true"  -> TmBool (true)
  | `Atom "false" -> TmBool (false)
  | `Atom  n      -> TmNum  (Float.of_string n)
  | `List[ `Atom "choice" ; `Atom gp ; e1 ; e2] ->
     match gp with
     | "?" ->  TmChoice (GProbDynamic, parse e1, parse e2 )
     | _   ->  TmChoice (GProb (Float.of_string gp), parse e1, parse e2)
    
  | _ -> failwith (sprintf "Parsing Error")





let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)




let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)
