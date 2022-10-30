open Base
open Stdio

let%test "rev" = List.equal Int.equal (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]

let%expect_test "multi-block" =
  print_endline "Hello";
  [%expect {| Hello |}];
  print_endline "World!";
  [%expect {| World! |}]
;;
