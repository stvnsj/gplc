open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector
open Z3.FloatingPoint
open Printf
open Matrix

exception TestFailedException of string





(* Takes a list of expressions and produces an addition *)
let addition (ctx : context) (es : Expr.expr list) =
  (Arithmetic.mk_add ctx es)


(* make_const_list : context -> int -> sort -> expr list
   Returns a list of size members *)
let rec make_const_list (ctx : context) (size : int) (sort : Sort.sort) =
  match size with
  | 0 -> []
  | _ -> (mk_fresh_const ctx "x" sort) :: (make_const_list ctx (size - 1) sort)

let mk_gprob_expr (ctx : context) (sort : Sort.sort) (probability : string) : Expr.expr =
  match probability with
  | "?" -> (mk_fresh_const ctx "p" sort)
  |  _  -> (Arithmetic.Real.mk_numeral_s ctx probability)





(* Returns a rows x cols matrix of constants *)
let rec make_const_matrix : context -> int -> int -> Sort.sort -> Expr.expr matrix = 
  fun ctx rows cols sort -> 
  match rows with
  | 0 -> []
  | _ -> (make_const_list ctx cols sort)
         :: (make_const_matrix ctx (rows - 1) cols sort)




  
 

let rec make_equation (ctx : context) (lh : Expr.expr) (rh : Expr.expr) =
  (Boolean.mk_eq ctx lh rh)

(* Make a real number from an integer *)
let mk_num_i (ctx : context) (i : int) = Arithmetic.Real.mk_numeral_i ctx i

(* Make a real number from an string *)
let mk_num_s (ctx : context) (s : string) = (Arithmetic.Real.mk_numeral_s ctx s)


(********************************)
(* NUMERIC EXPRESSION FUNCTIONS *)
(********************************)
(** Returns an expression of 1 *)
let one  (ctx : context) = (Arithmetic.Real.mk_numeral_i ctx 1)
(** Returns an expression of 0 *)
let zero (ctx : context) = (Arithmetic.Real.mk_numeral_i ctx 0)

(*************************)
(* CONSTRAINT FUNCTIONS  *)
(*************************)
(** Returns an expr with a unit interval constraint on expr [e] *)
let unit_interval_constraint : context -> Expr.expr -> Expr.expr =
  fun ctx e ->
  let c1 = (Arithmetic.mk_le ctx (zero ctx) e) in
  let c2 = (Arithmetic.mk_ge ctx (one ctx) e)  in
  (Boolean.mk_and ctx [c1 ; c2])

(** Returns an expr where the list of expr [es] adds up to 1 *)
let add_one_constraint : context -> Expr.expr list -> Expr.expr =
  fun ctx es ->
  let addtn  = addition ctx es in
  Boolean.mk_eq ctx (one ctx) addtn


let null_constraint (ctx : context) (e : Expr.expr) : Expr.expr =
  (Boolean.mk_eq ctx (zero ctx) e)


let constraint_generator (c : context) (b : bool) (e : Expr.expr) =
  match b with
  | false -> (null_constraint c e)
  | true  -> (unit_interval_constraint c e)

let constraint_matrix (c : context) (bm : bool matrix) (em : Expr.expr matrix) : Expr.expr matrix =
  List.map2 (List.map2 (constraint_generator c)) bm em




(**
   @param gpl1 gradual probability list #1
   @param gpl2 gradual probability list #2
   @param bin_matrix boolean matrix relating the gradual types
     from the two distributions
 *)
let solve_coupling (gpl1 : string list) (gpl2 : string list) (bin_matrix : 'a matrix) =

   
  let cfg = [("model", "true"); ("proof", "false")] in

  (* Context *)
  let ctx = (mk_context cfg) in

  (* Variables and Numbers of system have Real sort *)
  let real_sort = (Arithmetic.Real.mk_sort ctx) in

  (* Solver *)
  let solver = (mk_solver ctx None) in

  (* Lengths of gradual probability lists *)
  let len1 = List.length gpl1 in
  let len2 = List.length gpl2 in

  (* Lists of gradual probability expressions *)
  let gp_expr_lst1 = List.map (mk_gprob_expr ctx real_sort) gpl1 in
  let gp_expr_lst2 = List.map (mk_gprob_expr ctx real_sort) gpl2 in

  (* Constraint: Both lists must add up to 1 *)
  let add_one_c1  = (add_one_constraint ctx gp_expr_lst1) in
  let add_one_c2  = (add_one_constraint ctx gp_expr_lst2) in

  (* Constraint: Each gradual probability must be in the unit interval [0,1] *)
  let unit_interval_c1 = (List.map (unit_interval_constraint ctx) gp_expr_lst1) in
  let unit_interval_c2 = (List.map (unit_interval_constraint ctx) gp_expr_lst2) in 
  

  let matrix1  = (make_const_matrix ctx len1 len2 real_sort) in
  let matrix2  = transpose matrix1 in

  (* Constraint matrix *)
  let c_matrix = constraint_matrix ctx bin_matrix matrix1 in

  let add1 = List.map (addition ctx) matrix1 in
  let add2 = List.map (addition ctx) matrix2 in

  let eq1  = List.map2 (make_equation ctx) gp_expr_lst1 add1 in
  let eq2  = List.map2 (make_equation ctx) gp_expr_lst2 add2 in

  let constraint_list = eq1 @ eq2
                        @ (List.flatten c_matrix)
                        @ [add_one_c1 ; add_one_c2]
                        @ unit_interval_c1
                        @ unit_interval_c2 in
  
  Solver.add solver constraint_list ;

  match Solver.check solver [] with
  | SATISFIABLE ->

     let model = Option.get (Solver.get_model solver) in
     
     let solution_matrix = matrix_to_string
                             matrix1
                             (fun x -> Model.eval model x true |> Option.get |> Arithmetic.Real.numeral_to_string) in

     (* Printf.printf *)
     (*   "\n\n model = %s\n\n\n\nSolution Matrix = \n\n%s\n\n" *)
     (*   (Model.to_string model) solution_matrix *)
     printf "\nCoupling:\n============\n%s\n" solution_matrix;
     true

  | _ ->
     Printf.printf "\n\nSystem has no solution\n\n";
     false
                                                        


  



(* let myfunction () = *)
  
(*   let cfg = [("model", "true"); ("proof", "false")] in *)
(*   let ctx = (mk_context cfg) in *)
(*   let real_sort = (Arithmetic.Real.mk_sort ctx) in *)

(*   let constants = (make_const_list ctx 20 real_sort) in *)

(*   let x = (mk_fresh_const ctx "x" real_sort) in *)
(*   let y = x in *)

  
(*   let constants_sum = (addition ctx constants) in *)
  

(*   let n = (mk_num_i ctx 5) in *)

(*   let equation =  (Boolean.mk_eq ctx n constants_sum) in   *)

(*   let lower_bound = (Arithmetic.Real.mk_numeral_i ctx 0) in    (\* lower_bound real number *\) *)
(*   let upper_bound = (Arithmetic.Real.mk_numeral_i ctx 1) in   (\* lower_bound real number *\) *)




(*   let add_interval_constraint  = fun constant -> *)
(*     [(Arithmetic.mk_lt ctx constant upper_bound) ; *)
(*      (Arithmetic.mk_gt ctx constant lower_bound)] in *)
  
  

   

(*   (\**********\) *)
(*   (\* SOLVER *\) *)
(*   (\**********\) *)
(*   let solver = (mk_solver ctx None) in *)

  
(*   let n_list = List.map (mk_num_i ctx) [10; 20] in *)
(*   let m_list = List.map (mk_num_i ctx) [15; 7; 8] in *)

(*   let matrix1  = (make_const_matrix ctx 2 3 real_sort) in *)
(*   let matrix2  = transpose matrix1 in *)

(*   let add1 = List.map (addition ctx) matrix1 in *)
(*   let add2 = List.map (addition ctx) matrix2 in *)

(*   let eq1  = List.map2 (make_equation ctx) n_list add1 in *)
(*   let eq2  = List.map2 (make_equation ctx) m_list add2 in *)

 
(*   let constraint_list = List.flatten (List.map (List.map (Arithmetic.mk_le ctx lower_bound)) matrix1) in  *)

(*   Solver.add solver (eq1 @ eq2 @ constraint_list) ; *)

(*   match Solver.check solver [] with *)
(*   | SATISFIABLE -> *)

(*      let model = Option.get (Solver.get_model solver) in *)
     
(*      let solution_matrix = matrix_to_string  matrix1   (fun x -> Model.eval model x true |> Option.get |> Arithmetic.Real.numeral_to_string) in *)

(*      Printf.printf *)
(*        "\n\n model = %s\n\n\n\nSolution Matrix = \n\n%s\n\n" *)
(*        (Model.to_string model) solution_matrix *)

(*   | _ -> Printf.printf "\n\nSystem has no solution\n\n"; *)
         
(*   (\* Gc.full_major (); *\) *)

(*   (\* match Solver.check solver [] with *\) *)
(*   (\* | SATISFIABLE -> *\) *)

(*   (\*    let model = Option.get (Solver.get_model solver) in *\) *)
(*   (\*    let x_val = Model.eval model x true |> Option.get |> Arithmetic.Real.numeral_to_string in *\) *)
(*   (\*    let y_val = Model.eval model y true |> Option.get |> Arithmetic.Real.numeral_to_string in *\) *)
(*   (\*    let z_val = Model.eval model z true |> Option.get |> Arithmetic.Real.numeral_to_string in *\) *)
(*   (\*    Printf.printf *\) *)
(*   (\*      "\n\n---------------\nSolution: \nx = %s, \ny = %s, \nz = %s,\n model = %s\n---------------\n" *\) *)
(*   (\*      x_val y_val z_val (Model.to_string model) *\) *)

(*   (\* | _ -> Printf.printf "ERROR"; *\) *)
