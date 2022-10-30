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
  | first :: rest -> if k = 0 then first else nth rest (k - 1)
;;

let%test_unit "003 N'th element of a list" =
  [%test_eq: string option] (List.nth [ "a"; "b"; "c"; "d"; "e" ] 2) (Some "c");
  [%test_eq: string option] (List.nth [ "a" ] 2) None
;;
