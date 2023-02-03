open! Base

(* Working with Lists *)

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
    | h :: t -> if count = n then t else h :: aux (count + 1) t
  in
  aux 0 l
;;

let%test_unit "020 Remove the K'th element from a list" =
  [%test_eq: string list] (remove_at 1 [ "a"; "b"; "c"; "d" ]) [ "a"; "c"; "d" ]
;;

let insert_at x n l =
  let rec aux count = function
    | [] -> [ x ]
    | h :: t -> if n <= count then x :: h :: t else h :: aux (count + 1) t
  in
  aux 0 l
;;

let%test_unit "021 Insert an element at a given position into a list" =
  [%test_eq: string list]
    (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])
    [ "a"; "alfa"; "b"; "c"; "d" ];
  [%test_eq: string list]
    (insert_at "alfa" 7 [ "a"; "b"; "c"; "d" ])
    [ "a"; "b"; "c"; "d"; "alfa" ]
;;

let range a b =
  let rec aux acc x y = if x > y then acc else aux (x :: acc) (x + 1) y in
  if a <= b then List.rev (aux [] a b) else aux [] b a
;;

let%test_unit "022 Create a list containing all integers within a given range" =
  [%test_eq: int list] (range 4 9) [ 4; 5; 6; 7; 8; 9 ];
  [%test_eq: int list] (range 9 4) [ 9; 8; 7; 6; 5; 4 ]
;;

let rand_select l n =
  let rec extract acc n = function
    | [] -> failwith "extract"
    | h :: t -> if n = 0 then h, acc @ t else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len =
    let i = Random.int len in
    let picked, rest = extract [] i list in
    picked, rest
  in
  let rec aux n acc list len =
    if n = 0
    then acc
    else (
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1))
  in
  aux (min n (List.length l)) [] l (List.length l)
;;

let%test_unit "023 Extract a given number of randomly selected elements from a list" =
  let l = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] in
  let r = rand_select l 3 in
  [%test_eq: int] (List.length r) 3;
  List.iter r ~f:(fun x ->
    [%test_result: bool] ~expect:true (List.mem l x ~equal:String.equal))
;;

let lotto_select n m =
  let rec aux acc count =
    if count = n then acc else aux (Random.int m :: acc) (count + 1)
  in
  aux [] 0
;;

let%test_unit "024 Lotto: Draw N different random numbers from the set 1..M" =
  let achieved = lotto_select 6 49 in
  [%test_eq: int] (List.length achieved) 6;
  List.iter achieved ~f:(fun x -> [%test_result: bool] ~expect:true (x >= 1 && x <= 49))
;;

let permutation list =
  let rec extract acc n = function
    | [] -> failwith "extract"
    | h :: t -> if n = 0 then h, acc @ t else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len =
    let i = Random.int len in
    let picked, rest = extract [] i list in
    picked, rest
  in
  let rec aux acc = function
    | [] -> acc
    | l ->
      let picked, rest = extract_rand l (List.length l) in
      aux (picked :: acc) rest
  in
  aux [] list
;;

let%test_unit "025 Generate a random permutation of the elements of a list" =
  let original = [ "a"; "b"; "c"; "d"; "e"; "f" ] in
  let achieved = permutation original in
  [%test_eq: int] (List.length achieved) (List.length original);
  List.iter achieved ~f:(fun x ->
    [%test_result: bool] ~expect:true (List.mem original x ~equal:String.equal))
;;

let rec extract n list =
  if n <= 0
  then [ [] ]
  else (
    match list with
    | [] -> []
    | h :: t ->
      let with_h = List.map ~f:(fun l -> h :: l) (extract (n - 1) t) in
      let without_h = extract n t in
      with_h @ without_h)
;;

let%test_unit "026 Generate the combinations of K distinct objects chosen from the N \
               elements of a list"
  =
  [%test_eq: string list list]
    (extract 2 [ "a"; "b"; "c"; "d" ])
    [ [ "a"; "b" ]; [ "a"; "c" ]; [ "a"; "d" ]; [ "b"; "c" ]; [ "b"; "d" ]; [ "c"; "d" ] ]
;;

let group list sizes =
  let initial = List.map ~f:(fun size -> size, []) sizes in
  let prepend p list =
    let rec aux cons acc = function
      | [] -> cons [] acc
      | ((size, l) as h) :: t ->
        let acc = if size > 0 then cons ((size - 1, p :: l) :: t) acc else acc in
        aux (fun l acc -> cons (h :: l) acc) acc t
    in
    aux (fun l acc -> l :: acc) [] list
  in
  let rec aux = function
    | [] -> [ initial ]
    | h :: t -> List.concat (List.map ~f:(prepend h) (aux t))
  in
  let all = aux list in
  let complete = List.filter ~f:(List.for_all ~f:(fun (size, _) -> size = 0)) all in
  List.map ~f:(List.map ~f:snd) complete
;;

let%test_unit "027 Group the elements of a set into disjoint subsets" =
  [%test_eq: string list list list]
    (group [ "a"; "b"; "c"; "d" ] [ 2; 1 ])
    [ [ [ "a"; "b" ]; [ "c" ] ]
    ; [ [ "a"; "c" ]; [ "b" ] ]
    ; [ [ "b"; "c" ]; [ "a" ] ]
    ; [ [ "a"; "b" ]; [ "d" ] ]
    ; [ [ "a"; "c" ]; [ "d" ] ]
    ; [ [ "b"; "c" ]; [ "d" ] ]
    ; [ [ "a"; "d" ]; [ "b" ] ]
    ; [ [ "b"; "d" ]; [ "a" ] ]
    ; [ [ "a"; "d" ]; [ "c" ] ]
    ; [ [ "b"; "d" ]; [ "c" ] ]
    ; [ [ "c"; "d" ]; [ "a" ] ]
    ; [ [ "c"; "d" ]; [ "b" ] ]
    ];
  [%test_eq: string list list list] (group [ "a"; "b"; "c"; "d" ] [ 0 ]) [ [ [] ] ]
;;

let rec insert cmp e = function
  | [] -> [ e ]
  | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t
;;

let rec sort cmp = function
  | [] -> []
  | h :: t -> insert cmp h (sort cmp t)
;;

let length_sort lists =
  let lists = List.map ~f:(fun l -> l |> List.length, l) lists in
  let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
  List.map ~f:snd lists
;;

let rle list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [ x ] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 ((a, count + 1) :: acc) t
  in
  aux 0 [] list
;;

let frequency_sort lists =
  let lengths = List.map ~f:List.length lists in
  let list_to_freq = rle (sort compare lengths) in
  let by_freq =
    List.map
      ~f:(fun list ->
        List.Assoc.find_exn ~equal:Int.equal list_to_freq (list |> List.length), list)
      lists
  in
  let sorted = sort (fun a b -> compare (fst a) (fst b)) by_freq in
  List.map ~f:snd sorted
;;

let%test_unit "028 Sorting a list of lists according to length of sublists" =
  [%test_eq: string list list]
    (length_sort
       [ [ "a"; "b"; "c" ]
       ; [ "d"; "e" ]
       ; [ "f"; "g"; "h" ]
       ; [ "d"; "e" ]
       ; [ "i"; "j"; "k"; "l" ]
       ; [ "m"; "n" ]
       ; [ "o" ]
       ])
    [ [ "o" ]
    ; [ "d"; "e" ]
    ; [ "d"; "e" ]
    ; [ "m"; "n" ]
    ; [ "a"; "b"; "c" ]
    ; [ "f"; "g"; "h" ]
    ; [ "i"; "j"; "k"; "l" ]
    ];
  [%test_eq: string list list]
    (frequency_sort
       [ [ "a"; "b"; "c" ]
       ; [ "d"; "e" ]
       ; [ "f"; "g"; "h" ]
       ; [ "d"; "e" ]
       ; [ "i"; "j"; "k"; "l" ]
       ; [ "m"; "n" ]
       ; [ "o" ]
       ])
    [ [ "i"; "j"; "k"; "l" ]
    ; [ "o" ]
    ; [ "a"; "b"; "c" ]
    ; [ "f"; "g"; "h" ]
    ; [ "d"; "e" ]
    ; [ "d"; "e" ]
    ; [ "m"; "n" ]
    ]
;;

(* Arithmetic *)
let is_prime n =
  let rec aux = function
    | 1 -> true
    | x -> n < x * x || (n % x <> 0 && aux (x + 1))
  in
  1 <= n && aux 2
;;

let%test_unit "031 Determine whether a given integer number is prime" =
  [%test_eq: bool] (is_prime 1) true;
  [%test_eq: bool] (is_prime 7) true;
  [%test_eq: bool] (is_prime 12) false
;;

let rec gcd n m =
  if n < m
  then gcd m n
  else (
    let r = n % m in
    if r = 0 then m else gcd m r)
;;

let%test_unit "032 Determine the greatest common divisor of two positive integer numbers" =
  [%test_eq: int] (gcd 13 27) 1;
  [%test_eq: int] (gcd 20536 7826) 2
;;

let coprime n m = gcd n m = 1

let%test_unit "033 Determine whether two positive integer numbers are coprime" =
  [%test_eq: bool] (coprime 13 27) true;
  [%test_eq: bool] (not (coprime 20536 7826)) true
;;

let phi m = range 1 (m - 1) |> List.filter ~f:(coprime m) |> List.length

let%test_unit "034 Calculate Euler's totient function φ(m)" = [%test_eq: int] (phi 10) 4

let factors n =
  let rec aux d n =
    if n = 1 then [] else if n % d = 0 then d :: aux d (n / d) else aux (d + 1) n
  in
  aux 2 n
;;

let%test_unit "035 Determine the prime factors of a given positive integer" =
  [%test_eq: int list] (factors 315) [ 3; 3; 5; 7 ];
  [%test_eq: int list] (factors 1) []
;;

let factors n =
  let rec aux d n =
    if n = 1 then [] else if n % d = 0 then d :: aux d (n / d) else aux (d + 1) n
  in
  let primes = aux 2 n in
  let encode list =
    let rec aux count acc = function
      | [] -> []
      | [ x ] -> (x, count + 1) :: acc
      | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((a, count + 1) :: acc) t
    in
    List.rev (aux 0 [] list)
  in
  encode primes
;;

let%test_unit "036 Determine the prime factors of a given positive integer (2)" =
  [%test_eq: (int * int) list] (factors 315) [ 3, 2; 5, 1; 7, 1 ]
;;

let phi_improved n =
  let rec aux acc = function
    | [] -> acc
    | (p, m) :: t -> aux ((p - 1) * Int.pow p (m - 1) * acc) t
  in
  aux 1 (factors n)
;;

let%test_unit "037 Calculate Euler's totient function φ(m) (improved)" =
  [%test_eq: int] (phi_improved 10) 4;
  [%test_eq: int] (phi_improved 13) 12
;;

let rec all_primes a b =
  if a > b
  then []
  else (
    let rest = all_primes (a + 1) b in
    if is_prime a then a :: rest else rest)
;;

let%test_unit "039 A list of prime numbers" =
  [%test_eq: int] (all_primes 2 7920 |> List.length) 1000
;;

let goldbach n =
  let rec aux a =
    if 2 < a && is_prime a && is_prime (n - a) then a, n - a else aux (a + 1)
  in
  aux 0
;;

let%test_unit "040 Goldbach's conjecture" = [%test_eq: int * int] (goldbach 28) (5, 23)

let goldbach_list a b =
  range a b |> List.filter ~f:(fun x -> x % 2 = 0) |> List.map ~f:(fun x -> x, goldbach x)
;;

let%test_unit "041 A list of Goldbach compositions" =
  [%test_eq: (int * (int * int)) list]
    (goldbach_list 9 20)
    [ 10, (3, 7); 12, (5, 7); 14, (3, 11); 16, (3, 13); 18, (5, 13); 20, (3, 17) ]
;;

(* Logic and Codes *)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table2 (b1 : string) (b2 : string) exp =
  let rec aux (a : string) val_a (b : string) val_b = function
    | Var (x : string) ->
      if String.equal x a
      then val_a
      else if String.equal x b
      then val_b
      else failwith "Fail"
    | Not e -> not (aux a val_a b val_b e)
    | And (e1, e2) -> aux a val_a b val_b e1 && aux a val_a b val_b e2
    | Or (e1, e2) -> aux a val_a b val_b e1 || aux a val_a b val_b e2
  in
  [ true, true, aux b1 true b2 true exp
  ; true, false, aux b1 true b2 false exp
  ; false, true, aux b1 false b2 true exp
  ; false, false, aux b1 false b2 false exp
  ]
;;

let%test_unit "046 Truth tables for logical expressions (2 variables)" =
  [%test_eq: (bool * bool * bool) list]
    (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))))
    [ true, true, true; true, false, true; false, true, false; false, false, false ]
;;

let table vars exp =
  let rec eval var_to_val = function
    | Var x -> List.Assoc.find_exn var_to_val ~equal:String.equal x
    | Not e -> not (eval var_to_val e)
    | And (e1, e2) -> eval var_to_val e1 && eval var_to_val e2
    | Or (e1, e2) -> eval var_to_val e1 || eval var_to_val e2
  in
  let rec table_make acc vars exp =
    match vars with
    | [] -> [ List.rev acc, eval acc exp ]
    | h :: t -> table_make ((h, true) :: acc) t exp @ table_make ((h, false) :: acc) t exp
  in
  table_make [] vars exp
;;

let%test_unit "048 Truth tables for logical expressions" =
  [%test_eq: ((string * bool) list * bool) list]
    (table [ "a"; "b" ] (And (Var "a", Or (Var "a", Var "b"))))
    [ [ "a", true; "b", true ], true
    ; [ "a", true; "b", false ], true
    ; [ "a", false; "b", true ], false
    ; [ "a", false; "b", false ], false
    ]
;;

let gray n =
  let rec aux k acc =
    if k = n
    then acc
    else (
      let first, second =
        List.fold_left
          ~init:([], [])
          ~f:(fun (acc1, acc2) x -> ("0" ^ x) :: acc1, ("1" ^ x) :: acc2)
          acc
      in
      aux (k + 1) (List.rev_append first second))
  in
  aux 1 [ "0"; "1" ]
;;

let%test_unit "049 Gray code" =
  [%test_eq: string list] (gray 1) [ "0"; "1" ];
  [%test_eq: string list] (gray 2) [ "00"; "01"; "11"; "10" ];
  [%test_eq: string list]
    (gray 3)
    [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]
;;

(* let%test_unit "050 Huffman code" = "skip" *)

(* Binary Trees *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree [@sexp_drop_default.compare]
[@@deriving sexp]

let rec compare_trees t1 t2 =
  match t1, t2 with
  | Node (_, l1, r1), Node (_, l2, r2) -> compare_trees l1 l2 && compare_trees r1 r2
  | Node _, Empty -> false
  | Empty, Node _ -> false
  | _ -> true
;;

let rec compare_tree_list l1 l2 =
  if List.length l1 <> List.length l2
  then false
  else (
    match l1, l2 with
    | h1 :: t1, h2 :: t2 -> compare_trees h1 h2 && compare_tree_list t1 t2
    | _ -> true)
;;

let add_trees
  (left : 'a binary_tree list)
  (right : 'a binary_tree list)
  (init : 'a binary_tree list)
  : 'a binary_tree list
  =
  let aux acc l = List.fold_left ~f:(fun a r -> Node ('x', l, r) :: a) ~init:acc right in
  List.fold_left ~f:aux ~init left
;;

let rec cbal_tree n =
  if n = 0
  then [ Empty ]
  else if n % 2 = 1
  then (
    let t = cbal_tree (n / 2) in
    add_trees t t [])
  else (
    (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree ((n / 2) - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees t1 t2 (add_trees t2 t1 []))
;;

let%test_unit "055 Construct completely balanced binary trees" =
  [%test_eq: bool]
    (compare_tree_list
       (cbal_tree 4)
       [ Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
       ; Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)))
       ; Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty))
       ; Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
       ])
    true
;;

let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror r1 l2
  | _ -> false
;;

let is_symmetric = function
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r
;;

let%test_unit "056 Symmetric binary trees" =
  [%test_eq: bool]
    (is_symmetric (Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))))
    true
;;

let construct list =
  let rec insert tree x =
    match tree with
    | Empty -> Node (x, Empty, Empty)
    | Node (y, l, r) ->
      if x = y
      then tree
      else if x < y
      then Node (y, insert l x, r)
      else Node (y, l, insert r x)
  in
  List.fold_left ~f:insert ~init:Empty list
;;

let%test_unit "057 Binary search trees (dictionaries)" =
  [%test_eq: bool]
    (compare_trees
       (construct [ 3; 2; 5; 7; 1 ])
       (Node
          ( 3
          , Node (2, Node (1, Empty, Empty), Empty)
          , Node (5, Empty, Node (7, Empty, Empty)) )))
    true;
  [%test_eq: bool] (is_symmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ])) true;
  [%test_eq: bool] (not (is_symmetric (construct [ 3; 2; 5; 7; 4 ]))) true
;;

let sym_cbal_trees n = List.filter ~f:is_symmetric (cbal_tree n)

let%test_unit "058 Generate-and-test paradigm" =
  [%test_eq: bool]
    (compare_tree_list
       (sym_cbal_trees 5)
       [ Node
           ( 'x'
           , Node ('x', Node ('x', Empty, Empty), Empty)
           , Node ('x', Empty, Node ('x', Empty, Empty)) )
       ; Node
           ( 'x'
           , Node ('x', Empty, Node ('x', Empty, Empty))
           , Node ('x', Node ('x', Empty, Empty), Empty) )
       ])
    true;
  [%test_eq: int] (List.length (sym_cbal_trees 57)) 256
;;

(* Multiway Trees *)

(* Graphs *)

(* Mischelaneous Problems *)
