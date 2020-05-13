(*
File: Saurav Bhattarai
Name: useful.ml
*)

exception IgnoreCase;;
exception NotImplemented;;


let print_bool b = if b then
                     print_string "true"
                   else
                     print_string "false"
;;

type piece  = SPACE | BLACK | WHITE | HOlE;;
(* prints the int list in the correct format *)
let print_int_list list =
  let _ = print_string "[" in
  let rec print_list2 list = match list with
      [] -> ()
    | x::xs -> let _ = print_int x in
               match xs with
                 [] -> ()
               | n -> let _ = print_string "; " in (print_list2 n) in
  let _ = print_list2 list in
  print_string "]\n";;

(* prints the float list in the correct format *)
let print_float_list list =
  let _ = print_string "[" in
  let rec print_list2 list = match list with
      [] -> ()
    | x::xs -> let _ = print_float x in
               match xs with
                 [] -> ()
               | n -> let _ = print_string "; " in (print_list2 n) in
  let _ = print_list2 list in
  print_string "]\n";;


(* prints the index of target element in the list, returns -1 if not found *)
let index list x =
  let rec linearsearch list x idx = match list with
    [] -> -1
  | y::ys -> if y = x then
               idx
             else
               linearsearch ys x (idx + 1)
  in linearsearch list x 0
;;

(* returns the element at given index, ignores the case with n out of range *)
let rec at list n =
  if n < 0 then
    raise IgnoreCase
  else
    match list, n with
      [], _ -> raise IgnoreCase
    | x::xs, 0 -> x
    | x::xs, n -> at xs (n - 1)
;;

(* returs if the element is in list *)
let elementof x list = (index list x != -1);;

(* returns true if every element in list1 is in list2 *)
let rec subseteq list1 list2 = match list1 with
    [] -> true
  | x::xs -> if elementof x list2 then
               subseteq xs list2
             else
               false
;;

(* returns the length of the list *)
let length list =
  let rec len list acc = match list with
      [] -> acc
    | x::xs -> len xs acc + 1
  in len list 0
;;


(* list like in python *)

type 'a list = Empty
             | Cons of 'a * 'a list
;;

let init () = Empty;;

let rec size list = match list with
    Empty -> 0
  | Cons (x, xs) -> 1 + (size xs)
;;

let is_empty list = match list with
    Empty -> true
  | _ -> false
;;

let head list = match list with
    Empty -> raise IgnoreCase
  | Cons (x, xs) -> x
;;

let rec tail list = match list with
    Empty -> raise IgnoreCase;
  | Cons (x, xs) -> ( match xs with
                        Empty -> x
                      | _ -> tail xs )
;;

let insert_head xs x = Cons(x, xs);;

let delete_head list = match list with
    Empty -> raise IgnoreCase;
  | Cons(x, xs) -> xs
;;

let rec insert_tail xs x = match xs with
    Empty -> Cons(x, Empty)
  | Cons(y, ys) -> Cons(x, insert_tail ys x)
;;

let rec delete_tail list = match list with
    Empty -> raise IgnoreCase;
  | Cons(x, xs) -> ( match xs with
                       Empty -> Empty
                     | _ -> Cons(x, delete_tail xs) )
;;

let reverse list =
  let rec reverse2 list acc = match list with
      Empty -> acc
    | Cons(x, xs) -> reverse2 xs (insert_head acc x)
  in reverse2 list Empty
;;


