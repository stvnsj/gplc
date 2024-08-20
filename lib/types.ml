

open Matrix
exception MyException of string


(*****************************************)
(* Gradual Types and Gradual Probability *)
(*****************************************)

(* Gradual Probability type. *)
type gprob =
  | GProb of float
  | GProbDynamic


(* Gradual Type type *)
type gtype =
  | GTReal
  | GTBool
  | GTDynamic
  | GTFun of gtype * gdtype


(* Gradual Distribution Type type *)
and gdtype = GDType of (gtype list) * (gprob list)



(******************************************)
(* Formula Types and Symbolic Probability *)
(******************************************)

(* Symbolic Probability type *)
type sprob  = FProb of float | TVar of float * int * int

(* Formula Simple Type type *)
type fstype = FTReal | FTBool | FTDynamic




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
  



(**************************************)
(* Gradual Types Consistency Function *)
(**************************************)

let gtype_consistency : gtype -> gtype -> bool =
  fun gt1 gt2 -> 
  match (gt1 , gt2) with
  | (GTDynamic, _)
    | (_ , GTDynamic) 
    | (GTReal, GTReal)
    | (GTBool, GTBool) -> true
  | _ -> false


let rec consistency_matrix (gdt1 : gdtype) (gdt2 : gdtype) =
  match gdt1 with
  | GDType ([] , []) -> []
  | GDType (t :: ts , _ :: ps) ->
     (
       let GDType (ttss, _) = gdt2 in
       (List.map (gtype_consistency t) ttss) ::
             (consistency_matrix (GDType (ts , ps)) gdt2)

     )
  | _ -> raise (MyException "ERROR")



let get_gdt_prob_list (gdt : gdtype) =
  let GDType (_ , ps) = gdt in
  ps

let get_gtd_typ_list (gdt : gdtype) =
  let GDType (ts , _) = gdt in
  ts
  

    



(** [pp_gprob gp] returns string representation of [gp] *)
let pp_gprob (gp : gprob) : string =
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

  
  
 
  
