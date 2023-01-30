open Base

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
    | x -> x * x > n || (n mod x <> 0 && aux (x + 1))
  in
  n > 1 && aux 2
let unit_test "031 Determine whether a given integer number is prime" =
  [%test_eq: bool] (is_prime 1) true;
  [%test_eq: bool] (is_prime 7) true;
  [%test_eq: bool] (is_prime 12) false

(* Logic and Codes *)

(* Binary Trees *)

(* Multiway Trees *)

(* Graphs *)

(* Mischelaneous Problems *)
