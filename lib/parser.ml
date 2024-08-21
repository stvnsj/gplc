
open Printf
open CCSexp
open List
open Ast
open Types



let parse_type : sexp -> gtype =
  fun sexp ->
  match sexp with
  | `Atom "Real" -> GTReal
  | `Atom "Bool" -> GTBool
  | `Atom "?"    -> GTDynamic
  | _ -> failwith "parse_type ERROR"

let parse_prob : sexp -> gprob =
  fun sexp ->
  match sexp with
  | `Atom "?" -> GProbDynamic
  | `Atom  p  -> GProb (Float.of_string p)
  | _ -> failwith "parse_prob ERROR"





let rec parse_distribution_type : sexp -> gtype list * gprob list =
  fun sexp -> 
  match sexp with
  | `List ((`List [ t ; p]) :: tps ) ->
     let gt = parse_type t in
     let gp = parse_prob p in
     let (gts , gps) = parse_distribution_type (`List tps) in
     (gt :: gts , gp :: gps)
  | `List [] -> ([],[])
  | _ -> failwith "parse_distribution_type ERROR"
  



(*
  Proceses program syntax and returns an AST
 *)
let rec parse (sexp : sexp) : expr =
  match sexp with
    
  | `Atom "true"  -> TmBool (true)
  | `Atom "false" -> TmBool (false)
  | `Atom  n      -> TmNum  (Float.of_string n)

  (* Probabilitic Binary Choice Operation *)
  | `List[ `Atom "choice" ; `Atom gp ; e1 ; e2] ->
     (match gp with
     | "?" ->  TmChoice (GProbDynamic, parse e1, parse e2 )
     | _   ->  TmChoice (GProb (Float.of_string gp), parse e1, parse e2))

  | `List[ `Atom "::" ; e ; dt] ->
     (match dt with
      | `Atom s ->
         let gt  = parse_type dt in
         let gdt = GDType([gt],[(GProb 1.0)]) in
         TmAscr (parse e, gdt )
      | `List l -> 
         let (gtlst , gplst) = parse_distribution_type dt in
         TmAscr (parse e, GDType (gtlst,gplst)))


    
  | _ -> failwith (sprintf "parse Error")

  



let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)




let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)
