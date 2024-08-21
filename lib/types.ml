

open Matrix
open Solver
exception MyException of string


(*****************************************)
(* Gradual Types and Gradual Probability *)
(*****************************************)

(** Gradual Probability type. *)
type gprob =
  | GProb of float (* Explicit probability *)
  | GProbDynamic (* Dynamic Probability *)


(** Gradual Type type *)
type gtype =
  | GTReal (* Real Number type *)
  | GTBool (* Boolean Number type *)
  | GTDynamic (* Dynamic type *)
  | GTFun of gtype * gdtype (* Function type *)


(** Gradual Distribution Type type *)
and gdtype =
  | GDType of (gtype list) * (gprob list)



(******************************************)
(* Formula Types and Symbolic Probability *)
(******************************************)

(** Symbolic Probability type *)
type sprob  =
  | FProb of float
  | TVar of float * int * int

(** Formula Simple Type type *)
type fstype =
  | FTReal
  | FTBool
  | FTDynamic



(**************************)
(* PRETTY PRINT FUNCTIONS *)
(**************************)

(** [pp_gprob gp] returns string representation of [gp] *)
let pp_gprob : gprob -> string =
  fun gp -> 
  match gp with 
  | GProb p ->  Float.to_string p
  | GProbDynamic -> "?"

(** [pp_gtype gt] returns string representation of [gt] *)
let rec pp_gtype (gt : gtype) : string =
  match gt with
  | GTReal -> "Real"
  | GTBool -> "Bool"
  | GTDynamic -> "?"
  | GTFun (gt, gdt) ->
     Printf.sprintf
       "%s -> %s"
       (pp_gtype gt)
       (pp_gdtype gdt)

(** [pp_gdtype gdt] returns string representation of [gdt] *)
and pp_gdtype (gdt : gdtype) : string =
  match gdt with
  | GDType ([],[]) -> ""
  | GDType ((t :: ts) , (p :: ps)) ->
     Printf.sprintf
       "[%s : %s]%s"
       (pp_gtype t)
       (pp_gprob p)
       (pp_gdtype (GDType (ts , ps)))
  | _ ->  raise (MyException "Something went wrong!")

  
  




(******************************************************)
(* GRADUAL TYPES AND GRADUAL PROBABILITIES OPERATIONS *)
(******************************************************)

(** [op2_gprob] lifts a binary operation between floats to gradual
    probabilities [gp1] and [gp2] *)
let op2_gprob : (float -> float -> float) -> gprob -> gprob -> gprob =
  fun op gp1 gp2 ->
  match (gp1 , gp2) with
  | ((GProb p1) , (GProb p2)) -> GProb (op p1 p2)
  | (GProbDynamic , _) | (_ , GProbDynamic) -> GProbDynamic


let add_gdtype : gdtype -> gdtype -> gdtype =
  fun gdt1 gdt2 ->
  let GDType (tl1 , pl1) = gdt1 in
  let GDType (tl2 , pl2) = gdt2 in
  GDType ( tl1 @ tl2 , pl1 @ pl2)


let scale_gdtype : gprob -> gdtype -> gdtype =
  fun gp gdt ->
  let GDType(gtlist, gplist) = gdt in
  let scaled_gplist = List.map (op2_gprob ( *. ) gp) gplist in
  GDType (gtlist , scaled_gplist)


let gprob_comp : gprob -> gprob =
  fun gp ->
  match gp with
  | GProbDynamic -> gp
  | GProb p -> GProb (1. -. p)

let get_gdt_prob_list (gdt : gdtype) =
  let GDType (_ , ps) = gdt in
  ps

let get_gtd_typ_list (gdt : gdtype) =
  let GDType (ts , _) = gdt in
  ts
  

    
  



(**************************************)
(* GRADUAL TYPES CONSISTENCY FUNCTION *)
(**************************************)

let rec gtype_consistency : gtype -> gtype -> bool =
  fun gt1 gt2 -> 
  match (gt1 , gt2) with
  | (GTDynamic, _)
    | (_ , GTDynamic) 
    | (GTReal, GTReal)
    | (GTBool, GTBool) -> true
  | (GTFun(t1 , dt1), GTFun(t2, dt2)) ->
     let cond1 = gtype_consistency t1 t2 in
     let cond2 = gdtype_consistency dt1 dt2 in
     cond1 && cond2
  | _ -> false

(**  *)
and gdtype_consistency : gdtype -> gdtype -> bool =
  fun gdt1 gdt2 ->
  let lst1 = List.map pp_gprob (get_gdt_prob_list gdt1) in 
  let lst2 = List.map pp_gprob (get_gdt_prob_list gdt2) in
  let cmatrix = consistency_matrix gdt1 gdt2 in
  solve_coupling lst1 lst2 cmatrix



(** [consistency_matrix] computes with the consistency relation
    between the types in two distribution types [gdt1], [gdt2].
    For instance, the ditribution types (Bool, Real, ?) (?, Bool)
    produce the matrix:

    types | ?    |  Bool  |
    -----------------------
    Bool  | true |  true  |      Consistency Relation Matrix
    Real  | true |  false |

    This matrix is used to determine which elements of the computed
    coupling must be null (since they cannot relate inconsistent types).
 *)
and consistency_matrix : gdtype -> gdtype -> bool matrix =
  fun gdt1 gdt2 -> 
  match gdt1 with
  | GDType ([] , []) -> []
  | GDType (t :: ts , _ :: ps) ->
     let GDType (ttss, _) = gdt2 in
     (List.map (gtype_consistency t) ttss) :: (consistency_matrix (GDType (ts , ps)) gdt2)
  | _ -> raise (MyException "ERROR")


  




 
  
