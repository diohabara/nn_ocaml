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

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lis =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] lis)
;;

let%test_unit "007 Flatten a list" =
  [%test_eq: string list]
    (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
    [ "a"; "b"; "c"; "d"; "e" ]
;;

let rec compress = function
  | a :: (b :: _ as t) -> if phys_equal a b then compress t else a :: compress t
  | smaller -> smaller
;;

let%test_unit "008 Eliminate duplicates" =
  [%test_eq: string list]
    (compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

let pack list =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if phys_equal a b
      then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] list)
;;

let%test_unit "009 Pack consecutive duplicates" =
  [%test_eq: string list list]
    (pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ])
    [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

let encode list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if phys_equal a b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)
;;

let%test_unit "010 Run-length encoding" =
  [%test_eq: (int * string) list]
    (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
  let create_tuple count element =
    if count = 1 then One element else Many (count, element)
  in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
      if phys_equal a b
      then aux (count + 1) acc t
      else aux 0 (create_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] list)
;;

let rle_equal x y =
  match x, y with
  | One x_val, One y_val -> phys_equal x_val y_val
  | Many (x_count, x_val), Many (y_count, y_val) ->
    x_count = y_count && phys_equal x_val y_val
  | _ -> false
;;

let%test "011 Modified run-length encoding" =
  List.equal
    rle_equal
    (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;