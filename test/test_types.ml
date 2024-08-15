(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

module Gplc = Gplc.Types



(* A module with functions to test *)
module To_test = struct
  let gdt = Gplc.GDType [(Gplc.GTReal, Gplc.GProbDynamic);
                         (Gplc.GTBool, (Gplc.GProb 0.3))]
  let gdist =  Gplc.pp_gdtype gdt
  let gtreal = Gplc.pp_gtype Gplc.GTReal
  let gtbool = Gplc.pp_gtype Gplc.GTBool
  let gtfun  = Gplc.pp_gtype (Gplc.GTFun  (Gplc.GTReal, gdt))
  let gtdynamic = Gplc.pp_gtype Gplc.GTDynamic
  let gprob  = Gplc.pp_gprob (Gplc.GProb 0.5)
  let gprobdyn  = Gplc.pp_gprob Gplc.GProbDynamic
end

(* The tests *)
let test_real () = Alcotest.(check string) "same type string" "Real" To_test.gtreal
let test_bool () = Alcotest.(check string) "same type string" "Bool" To_test.gtbool
let test_fun () = Alcotest.(check string) "same type string" "Real -> [Real : ?][Bool : 0.3]" To_test.gtfun
let test_dynamic () = Alcotest.(check string) "same type string" "?" To_test.gtdynamic
let test_prob () = Alcotest.(check string) "same type string" "0.5" To_test.gprob
let test_prob_dyn () = Alcotest.(check string) "same type string" "?" To_test.gprobdyn
let test_dist () = Alcotest.(check string) "same type string" "[Real : ?][Bool : 0.3]" To_test.gdist

(* Run it *)
let () =
  let open Alcotest in
  run "Pretty Prints" [
      
      "Gradual Type", [
        test_case "Real Test" `Quick test_real;
        test_case "Bool Test" `Quick test_bool;
        test_case "Function Test" `Quick test_fun;
        test_case "Dynamic Test" `Quick test_dynamic;
      ];
      
      "Gradual Probability", [
        test_case "Known Prob" `Quick test_prob;
        test_case "Dynamic Prob" `Quick test_prob_dyn;
        ];

      "Gradual Distribution Type", [
          test_case "Distribution Test" `Quick test_dist;
        ];
    ]
