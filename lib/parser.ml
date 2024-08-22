open Printf
open CCSexp
open List
open Ast
open Types


(** Parse a gradual type from [sexp] *)
let parse_type : sexp -> gtype =
  fun sexp ->
  match sexp with
  | `Atom "Real" -> GTReal
  | `Atom "Bool" -> GTBool
  | `Atom "?"    -> GTDynamic
  | _ -> failwith "parse_type ERROR"


(** Parse a gradual probability from [sexp] *)
let parse_prob : sexp -> gprob =
  fun sexp ->
  match sexp with
  | `Atom "?" -> GProbDynamic
  | `Atom  p  -> GProb (Float.of_string p)
  | _ -> failwith "parse_prob ERROR"


(** Parse a gradual distribution type from [sexp] *)
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


(** [parse] takes a symbolic expression program [sexp] *)
let rec parse : sexp -> expr =
  fun sexp -> 
  match sexp with
    
  | `Atom "true"  -> TmBool (true) (* boolean literal value *)
  | `Atom "false" -> TmBool (false) (* boolean literal value *)
  | `Atom  n      -> TmNum  (Float.of_string n) (* numeric literal value *)

  | `List[ `Atom "choice" ; `Atom gp ; e1 ; e2] -> (* Probabilitic Binary Choice *)
     (match gp with
     | "?" ->  TmChoice (GProbDynamic, parse e1, parse e2 )
     | _   ->
        let r = Float.of_string gp in
        if r >= 0. && r <= 1.
        then TmChoice (GProb r, parse e1, parse e2)
        else failwith "Probability must be in interval [0,1]")

  | `List[ `Atom "::" ; e ; t] -> (* Ascription *)
     (match t with
      | `Atom _ -> (* Gradual types Real, Bool, ? *)
         let gt  = parse_type t in
         TmAscr1 (parse e, gt )
      | `List _ -> (* Gradual distribution type *)
         let (gtlst , gplst) = parse_distribution_type t in
         TmAscr2 (parse e, GDType (gtlst,gplst)))    
  | _ -> failwith (sprintf "parse Error")


let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
 match CCSexp.parse_file filename with
 | Ok s -> s
 | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)


let sexp_from_string : string -> sexp =
  fun src -> 
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)
