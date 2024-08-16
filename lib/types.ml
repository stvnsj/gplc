
(*****************************************)
(* Gradual Types and Gradual Probability *)
(*****************************************)

type gprob = GProb of float | GProbDynamic
(** Gradual Probability type. *)

type gtype = GTReal | GTBool | GTDynamic | GTFun of gtype * gdtype
(** Gradual Type type *)

and gdtype = GDType of (gtype * gprob) list
(** Gradual Distribution Type type *)


(******************************************)
(* Formula Types and Symbolic Probability *)
(******************************************)


type sprob  = FProb of float | TVar of float * int * int
(** Symbolic Probability type *)

type fstype = FTReal | FTBool | FTDynamic
(** Formula Simple Type type *)


(**************************************)
(* Gradual Types Consistency Function *)
(**************************************)

let gtype_consistency (gt1 : gtype)  (gt2: gtype) : bool =
  match (gt1 , gt2) with
  | (GTDynamic, _)
    | (_ , GTDynamic) 
    | (GTReal, GTReal)
    | (GTBool, GTBool) -> true
  | _ -> false





















(* Pretty Printing Functions *)
let pp_gprob (gp : gprob) : string =
  (** [pp_gprob gp] returns string representation of [gp] *)
  match gp with 
  | GProb p -> Float.to_string p
  | GProbDynamic -> "?"

let rec pp_gtype (gt : gtype) : string =
  (** [pp_gtype gt] returns string representation of [gt] *)
  match gt with
  | GTReal -> "Real"
  | GTBool -> "Bool"
  | GTDynamic -> "?"
  | GTFun (gt, gdt) ->
     Printf.sprintf
       "%s -> %s"
       (pp_gtype gt)
       (pp_gdtype gdt)

and pp_gdtype (gdt : gdtype) : string =
  (** [pp_gdtype gdt] returns string representation of [gdt] *)
  match gdt with
  | GDType [] -> ""
  | GDType ((gt , p) :: xs) ->
     Printf.sprintf
       "[%s : %s]%s"
       (pp_gtype gt)
       (pp_gprob p)
       (pp_gdtype (GDType xs))

  
  
 
  
