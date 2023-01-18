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

let decode list =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
  List.rev (aux [] list)
;;

let%test_unit "012 Decode a run-length encoded list" =
  [%test_eq: string list]
    (decode
       [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ])
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

let encode list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> rle count x :: acc
    | a :: (b :: _ as t) ->
      if phys_equal a b then aux (count + 1) acc t else aux 0 (rle count a :: acc) t
  in
  aux 0 [] (List.rev list)
;;

let%test "013 Run-length encoding of a list (direct solution)" =
  List.equal
    rle_equal
    (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | x :: t -> aux (x :: x :: acc) t
  in
  aux [] (List.rev list)
;;

let%test_unit "014 Duplicate the elements of a list" =
  [%test_eq: string list]
    (duplicate [ "a"; "b"; "c"; "c"; "d" ])
    [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
;;

let replicate list count =
  let rec many x n = if n = 0 then [] else x :: many x (n - 1) in
  let rec aux acc = function
    | [] -> acc
    | x :: t -> aux (many x count @ acc) t
  in
  aux [] (List.rev list)
;;

let%test_unit "015 Replicate the elements of a list a given number of times" =
  [%test_eq: string list]
    (replicate [ "a"; "b"; "c" ] 3)
    [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;

let drop list n =
  let rec aux acc count = function
    | [] -> acc
    | a :: t -> if count + 1 = n then aux acc 0 t else aux (a :: acc) (count + 1) t
  in
  List.rev (aux [] 0 list)
;;

let%test_unit "016 Drop every N'th element from a list" =
  [%test_eq: string list]
    (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
;;

let split list count =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l else aux (i - 1) (h :: acc) t
  in
  aux count [] list
;;

let%test_unit "017 Split a list into two parts; the length of the first part is given" =
  [%test_eq: string list * string list]
    (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]);
  [%test_eq: string list * string list]
    (split [ "a"; "b"; "c"; "d" ] 5)
    ([ "a"; "b"; "c"; "d" ], [])
;;

let slice list s e =
  let rec aux i acc = function
    | [] -> acc
    | h :: t -> if s <= i && i <= e then aux (i + 1) (h :: acc) t else aux (i + 1) acc t
  in
  List.rev (aux 0 [] list)
;;

let%test_unit "018 Extract a slice from a list" =
  [%test_eq: string list]
    (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)
    [ "c"; "d"; "e"; "f"; "g" ]
;;

let rotate list n =
  let len = List.length list in
  let n = if len = 0 then 0 else ((n % len) + len) % len in
  if n = 0
  then list
  else (
    let a, b = split list n in
    b @ a)
;;

let%test_unit "019 Rotate a list N places to the left" =
  [%test_eq: string list]
    (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
    [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
;;

let remove_at n l =
  let rec aux count = function
    | [] -> []
    | h :: t -> if count = n then t else h :: (aux (count + 1) t)
  in
  aux 0 l
;;

let%test_unit "020 Remove the K'th element from a list" =
  [%test_eq: string list]
  (remove_at 1 ["a"; "b"; "c"; "d"])
  ["a"; "c"; "d"]
;;
