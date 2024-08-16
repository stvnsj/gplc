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

exception TestFailedException of string



let myfunction () =
  
  	let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = (mk_context cfg) in

        let x = (Arithmetic.Real.mk_const_s ctx "x" ) in   (* Create a double constant x *)
        let y = (Arithmetic.Real.mk_const_s ctx "y" ) in   (* Create a double constant y *)
        let z = (Arithmetic.Real.mk_const_s ctx "z" ) in   (* Create a double constant z *)
        
        let w = (Arithmetic.mk_add ctx [x; y; z])  in        (* Sum of three constants x + y + z *)
        let n = (Arithmetic.Real.mk_numeral_i ctx 96) in
        let c = (Boolean.mk_eq ctx n w ) in                  (* Create equality  n = x + y + z *)

        let lower_bound = (Arithmetic.Real.mk_numeral_i ctx 1) in   (* lower_bound real number *)
        let upper_bound = (Arithmetic.Real.mk_numeral_i ctx 50) in   (* lower_bound real number *)

        let c1   = (Arithmetic.mk_gt ctx x lower_bound) in          (* list of constraints *)
        let c2   = (Arithmetic.mk_gt ctx y lower_bound) in
        let c3   = (Arithmetic.mk_gt ctx z lower_bound) in


        let c4   = (Arithmetic.mk_lt ctx x upper_bound) in          (* list of constraints *)
        let c5   = (Arithmetic.mk_lt ctx y upper_bound) in
        let c6   = (Arithmetic.mk_lt ctx z upper_bound) in

        
        let c4   = (Boolean.mk_and ctx [c; c1; c2; c3; c4; c5; c6]) in


        let solver = (mk_solver ctx None) in
        
        Solver.add solver [ c4 ] ;

        match Solver.check solver [] with
        | SATISFIABLE ->
           Printf.printf "Test passed.\n" ;
           let model = Option.get (Solver.get_model solver) in
           let x_val = Model.eval model x true |> Option.get |> Arithmetic.Real.numeral_to_string in
           let y_val = Model.eval model y true |> Option.get |> Arithmetic.Real.numeral_to_string in
           let z_val = Model.eval model z true |> Option.get |> Arithmetic.Real.numeral_to_string in
           Printf.printf
             "\n\n--------------------------\n Solution: \nx = %s, \ny = %s \nz = %s\n\n-----------------------------\n"
             x_val y_val z_val
        | _ -> Printf.printf "ERROR";

        Printf.printf "Hola mundo!\n";
        
	Gc.full_major ()
