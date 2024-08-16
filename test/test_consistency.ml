
(* Build with `ocamlbuild -pkg alcotest simple.byte` *)
module Gplc = Gplc.Types


(* A module with functions to test *)
module To_test = struct
  let gtreal = (Gplc.gtype_consistency Gplc.GTReal Gplc.GTReal)
end


(* The tests *)

let test_real_real () = Alcotest.(check bool) "same bool"
                          true
                          (Gplc.gtype_consistency
                             Gplc.GTReal
                             Gplc.GTReal)

let test_bool_bool () = Alcotest.(check bool) "same bool"
                          true
                          (Gplc.gtype_consistency
                             Gplc.GTBool
                             Gplc.GTBool)

let test_dyn_bool () = Alcotest.(check bool) "same bool"
                          true
                          (Gplc.gtype_consistency
                             Gplc.GTDynamic
                             Gplc.GTBool)


let test_real_dyn () = Alcotest.(check bool) "same bool"
                          true
                          (Gplc.gtype_consistency
                             Gplc.GTReal
                             Gplc.GTDynamic)




let test_bool_real () = Alcotest.(check bool) "same bool"
                          false
                          (Gplc.gtype_consistency
                             Gplc.GTBool
                             Gplc.GTReal)


(* Run it *)
let () =
  let open Alcotest in
  
  run "Consistency" [
      
      "Consistent Gradual Type", [
        test_case "Real Real" `Quick test_real_real;
        test_case "Bool Bool" `Quick test_bool_bool;
        test_case "? Bool" `Quick test_dyn_bool;
        test_case "Real ?" `Quick test_real_dyn;
      ];

      "Inconsistent Gradual Types", [
          test_case "Bool Real" `Quick test_bool_real;
        ];

    ]
