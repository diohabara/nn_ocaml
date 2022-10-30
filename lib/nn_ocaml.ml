open Base

let rec last lis =
  match lis with
  | [ first ] -> Some first
  | _ :: rest -> last rest
  | _ -> None
;;

let%test_unit "001 Tail of a list" =
  [%test_eq: string option] (last [ "a"; "b"; "c"; "d" ]) (Some "d");
  [%test_eq: string option] (last []) None
;;

let rec last_two lis =
  match lis with
  | [ first; second ] -> Some (first, second)
  | _ :: rest -> last_two rest
  | _ -> None
;;

let%test_unit "002 Last two elements of a list" =
  [%test_eq: (string * string) option] (last_two [ "a"; "b"; "c"; "d" ]) (Some ("c", "d"));
  [%test_eq: (string * string) option] (last_two [ "a" ]) None
;;

let rec nth lis k =
  match lis with
  | [] -> None
  | first :: rest -> if k = 0 then Some first else nth rest (k - 1)
;;

let%test_unit "003 N'th element of a list" =
  [%test_eq: string option] (nth [ "a"; "b"; "c"; "d"; "e" ] 2) (Some "c");
  [%test_eq: string option] (nth [ "a" ] 2) None
;;

let rec length lis =
  match lis with
  | [] -> 0
  | _ :: rest -> 1 + length rest
;;

let%test_unit "004 Length of a list" =
  [%test_eq: int] (length [ "a"; "b"; "c" ]) 3;
  [%test_eq: int] (length []) 0
;;

let rev lis =
  let rec helper acc = function
    | [] -> acc
    | first :: rest -> helper (first :: acc) rest
  in
  helper [] lis
;;

let%test_unit "005 Reverse a list" =
  [%test_eq: string list] (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ]
;;

let is_palindrome lis = List.equal String.equal lis (rev lis)

let%test_unit "006 Palindrome" =
  [%test_eq: bool] (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]) true;
  [%test_eq: bool] (not (is_palindrome [ "a"; "b" ])) true
;;
