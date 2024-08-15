
(* Gradual Probability *)
type gprob = GProb of float | GProbDynamic

(* Gradual Types *)
type gtype = GTReal | GTBool | GTDynamic | GTFun of gtype * gdtype

(* Gradual Distribution Type *)
and gdtype = GDType of (gtype * gprob) list

let pp_gprob (gp : gprob) =
  match gp with 
  | GProb p -> Float.to_string p
  | GProbDynamic -> "?"

let rec pp_gtype (gt : gtype) =
  match gt with
  | GTReal -> "Real"
  | GTBool -> "Bool"
  | GTDynamic -> "?"
  | GTFun (gt, gdt) ->
     Printf.sprintf
       "%s -> %s"
       (pp_gtype gt)
       (pp_gdtype gdt)

and pp_gdtype (gdt : gdtype) =
  match gdt with
  | GDType [] -> ""
  | GDType ((gt , p) :: xs) ->
     Printf.sprintf
       "[%s : %s]%s"
       (pp_gtype gt)
       (pp_gprob p)
       (pp_gdtype (GDType xs))

  
  
 
  
