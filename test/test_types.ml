open Lib.Types



(* Gradual Probabilities *)
let gp1 = GProb 0.3
let gp2 = GProb 0.2
let gp3 = GProbDynamic



(* Gradual Distribution Types *)
let gdt1 = GDType (
               [GTReal      ; GTBool      ; GTDynamic],
               [(GProb 0.3) ; (GProb 0.4) ; (GProb 0.3)])

let gdt2 = GDType (
               [GTReal       ; GTBool     ],
               [GProbDynamic ; (GProb 0.4)])


let gdt3 = GDType (
               [GTReal      ; GTDynamic],
               [(GProb 0.2) ; (GProb 0.1)])





(* A module with functions to test *)
module To_test = struct
  let gdt = GDType ([ GTReal ; GTBool  ] ,
                    [GProbDynamic; (GProb 0.3)])
  let gdist =  pp_gdtype gdt
  let gtreal = pp_gtype GTReal
  let gtbool = pp_gtype GTBool
  let gtfun  = pp_gtype (GTFun  (GTReal, gdt))
  let gtdynamic = pp_gtype GTDynamic
  let gprob  = pp_gprob (GProb 0.5)
  let gprobdyn  = pp_gprob GProbDynamic


  

  (* Gradual Probabilities operation *)
  let gprob_operation1 = pp_gprob (op2_gprob ( +. ) gp1 gp2)
  let gprob_operation2 = pp_gprob (op2_gprob ( *. ) gp1 gp2)
  let gprob_operation3 = pp_gprob (op2_gprob ( *. ) gp1 gp3)

  (* Gradual Probability and Gradual Distribution Type operation *)
  let scale_gdt1 = pp_gdtype (scale_gdtype gp1 gdt1)
  let scale_gdt2 = pp_gdtype (scale_gdtype gp3 gdt1)
  let scale_gdt3 = pp_gdtype (scale_gdtype gp2 gdt2)

  (* Gradual Distribution Types addition *)
  let add_gdt_1  = pp_gdtype (add_gdtype gdt2 gdt3)
  
  
end

(* The tests *)
let test_real () = Alcotest.(check string) "same type string" "Real" To_test.gtreal
let test_bool () = Alcotest.(check string) "same type string" "Bool" To_test.gtbool
let test_fun () = Alcotest.(check string) "same type string" "Real -> [Real : ?][Bool : 0.3]" To_test.gtfun
let test_dynamic () = Alcotest.(check string) "same type string" "?" To_test.gtdynamic
let test_prob () = Alcotest.(check string) "same type string" "0.5" To_test.gprob
let test_prob_dyn () = Alcotest.(check string) "same type string" "?" To_test.gprobdyn
let test_dist () = Alcotest.(check string) "same type string" "[Real : ?][Bool : 0.3]" To_test.gdist

(* Gradual Probability operations tests *)
let test_gprob_op1 () = Alcotest.(check string) "same type string" "0.5" To_test.gprob_operation1
let test_gprob_op2 () = Alcotest.(check string) "same type string" "0.06" To_test.gprob_operation2
let test_gprob_op3 () = Alcotest.(check string) "same type string" "?" To_test.gprob_operation3

let test_scale_1 () = Alcotest.(check string) "same type string"
                        "[Real : 0.09][Bool : 0.12][? : 0.09]" To_test.scale_gdt1

let test_scale_2 () = Alcotest.(check string) "same type string"
                        "[Real : ?][Bool : ?][? : ?]" To_test.scale_gdt2

let test_scale_3 () = Alcotest.(check string) "same type string"
                        "[Real : ?][Bool : 0.08]" To_test.scale_gdt3



let test_add_1 () = Alcotest.(check string) "same type string"
                      "[Real : ?][Bool : 0.4][Real : 0.2][? : 0.1]" To_test.add_gdt_1

(* Run it *)
let () =
  let open Alcotest in
  run "Pretty Prints" [
      
      "Gradual Type", [
        test_case "Real Test"     `Quick test_real;
        test_case "Bool Test"     `Quick test_bool;
        test_case "Function Test" `Quick test_fun;
        test_case "Dynamic Test"  `Quick test_dynamic;
      ];
      
      "Gradual Probability", [
        test_case "Known Prob"   `Quick test_prob;
        test_case "Dynamic Prob" `Quick test_prob_dyn;
        ];

      "Gradual Distribution Type", [
          test_case "Distribution Test" `Quick test_dist;
        ];

      "Gradual Probability Operations", [
          test_case "grad prob add"  `Quick test_gprob_op1;
          test_case "grad prob mult" `Quick test_gprob_op2;
          test_case "grad prob mult" `Quick test_gprob_op3;
        ];

      "Scale Gradual Distribution Type",[
          test_case "0.3 * dgtype" `Quick test_scale_1;
          test_case "? * dgtype"   `Quick test_scale_2;
          test_case "0.2 * dgtype" `Quick test_scale_3;
        ];

      "Gradual Distribution Type addition",[
          test_case "gdt + gdt" `Quick test_add_1;
        ]
    ]
