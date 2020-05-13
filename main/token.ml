(* File: token.ml *)

exception IgnoreCase;;

let print_bool b = if b then
                     print_string "true"
                   else
                     print_string "false"
;;

let int_of_bool b = if b then
                      1
                    else
                      0
;;

let float_of_bool b = if b then
                        1.0
                      else
                        0.0
;;


type token = Int_tok of int
           | Float_tok of float
           | Bool_tok of bool
           | Id_tok of string
           | Plus_tok | Minus_tok | Times_tok | Div_tok
           | Is_Equ_tok | Is_Neq_tok | Equ_tok
           | If_tok | Else_tok
           | Lparen_tok | Rparen_tok
           | Lcomment_tok | Rcomment_tok
;;
           
exception IgnoreCase;;



let rec get_val id symtable = match id, symtable with
    _, [] -> raise IgnoreCase
  | Id_tok id1, x::xs -> (match x with
                            Id_tok id2, v -> if (id1 = id2) then 
                                                  v
                                             else
                                               get_val id xs
                          | _, _ -> raise IgnoreCase
                         )
  | _, _ -> raise IgnoreCase
;;

let print_token t = match t with
    Int_tok i -> print_int i
  | Float_tok f -> print_float f
  | Bool_tok b -> print_bool b
  | Id_tok id -> print_string ("Id_tok " ^ "\"" ^ id ^ "\"")
  | Plus_tok -> print_string ("Plus_tok")
  | Minus_tok -> print_string ("Minus_tok")
  | Times_tok -> print_string ("Times_tok")
  | Div_tok -> print_string ("Div_tok")
  | Is_Equ_tok -> print_string ("Is_Equ_tok")
  | Is_Neq_tok -> print_string ("Is_Neq_tok")
  | Equ_tok -> print_string ("Equ_tok")
  | If_tok -> print_string ("If_tok")
  | Else_tok -> print_string ("Else_tok")
  | Lparen_tok -> print_string ("Lparen_tok")
  | Rparen_tok -> print_string ("Rparen_tok")
  | Lcomment_tok -> print_string ("Lcomment_tok")
  | Rcomment_tok -> print_string ("Rcomment_tok")
;;

(*
 Write a print_tokens function. For instance print_tokens [Int_tok 5,
Float 3.1, Id_tok "num_heads", Plus_tok] prints
[Int_tok 5, Float 3.1, Id_tok "num_heads", Plus_tok]
on the console window.
*)

let print_tokens tokens = 
  let _ = print_string "[" in
  let rec pt tokens = match tokens with
      [] -> ()
    | x::xs -> let _ = print_token x in
               ( match xs with
                   [] -> ()
                 | _ -> let _ = print_string "; " in
                        pt xs
               )
  in
  let _ = pt tokens in
  print_string "]\n"
;;
    
(* print_tokens [Int_tok 5; Plus_tok; Float_tok 10.2; Id_tok "abc123"];; *)

let rec eval_plus expr1 expr2 symtable = match expr1, expr2 with
    Int_tok i1, Int_tok i2 -> Int_tok (i1 + i2)
  | Float_tok i1, Float_tok i2 -> Float_tok (i1 +. i2)
  | Int_tok i1, Float_tok i2 -> Float_tok (float_of_int(i1) +. i2)
  | Float_tok i1, Int_tok i2 -> Float_tok (i1 +. float_of_int(i2))
  | Bool_tok i1, Int_tok i2 -> Int_tok (int_of_bool(i1) + i2)
  | Int_tok i1, Bool_tok i2 -> Int_tok (i1 + int_of_bool(i2))
  | Bool_tok i1, Bool_tok i2 -> Int_tok (int_of_bool(i1) + int_of_bool(i2))
  | Bool_tok i1, Float_tok i2 -> Float_tok (float_of_bool(i1) +. i2)
  | Float_tok i1, Bool_tok i2 -> Float_tok (i1 +. float_of_bool(i2))
  | Id_tok i1, _ -> eval_plus (get_val expr1 symtable) expr2 symtable
  | _, Id_tok i2 -> eval_plus expr1 (get_val expr2 symtable) symtable
  | _, _ -> raise IgnoreCase
;;

let rec eval_minus expr1 expr2 symtable = match expr1, expr2 with
    Int_tok i1, Int_tok i2 -> Int_tok (i1 - i2)
  | Float_tok i1, Float_tok i2 -> Float_tok (i1 -. i2)
  | Int_tok i1, Float_tok i2 -> Float_tok (float_of_int(i1) -. i2)
  | Float_tok i1, Int_tok i2 -> Float_tok (i1 -. float_of_int(i2))
  | Bool_tok i1, Int_tok i2 -> Int_tok (int_of_bool(i1) - i2)
  | Int_tok i1, Bool_tok i2 -> Int_tok (i1 - int_of_bool(i2))
  | Bool_tok i1, Bool_tok i2 -> Int_tok (int_of_bool(i1) - int_of_bool(i2))
  | Bool_tok i1, Float_tok i2 -> Float_tok (float_of_bool(i1) -. i2)
  | Float_tok i1, Bool_tok i2 -> Float_tok (i1 -. float_of_bool(i2))
  | Id_tok i1, _ -> eval_minus (get_val expr1 symtable) expr2 symtable
  | _, Id_tok i2 -> eval_minus expr1 (get_val expr2 symtable) symtable
  | _, _ -> raise IgnoreCase
;;

let rec eval_times expr1 expr2 symtable = match expr1, expr2 with
    Int_tok i1, Int_tok i2 -> Int_tok (i1 * i2)
  | Float_tok i1, Float_tok i2 -> Float_tok (i1 *. i2)
  | Int_tok i1, Float_tok i2 -> Float_tok (float_of_int(i1) *. i2)
  | Float_tok i1, Int_tok i2 -> Float_tok (i1 *. float_of_int(i2))
  | Bool_tok i1, Int_tok i2 -> Int_tok (int_of_bool(i1) * i2)
  | Int_tok i1, Bool_tok i2 -> Int_tok (i1 * int_of_bool(i2))
  | Bool_tok i1, Bool_tok i2 -> Int_tok (int_of_bool(i1) * int_of_bool(i2))
  | Bool_tok i1, Float_tok i2 -> Float_tok (float_of_bool(i1) *. i2)
  | Float_tok i1, Bool_tok i2 -> Float_tok (i1 *. float_of_bool(i2))
  | Id_tok i1, _ -> eval_times (get_val expr1 symtable) expr2 symtable
  | _, Id_tok i2 -> eval_times expr1 (get_val expr2 symtable) symtable
  | _, _ -> raise IgnoreCase
;;

let rec eval_div expr1 expr2 symtable = match expr1, expr2 with
    Int_tok i1, Int_tok i2 -> Int_tok (i1 / i2)
  | Float_tok i1, Float_tok i2 -> Float_tok (i1 /. i2)
  | Int_tok i1, Float_tok i2 -> Float_tok (float_of_int(i1) /. i2)
  | Float_tok i1, Int_tok i2 -> Float_tok (i1 /. float_of_int(i2))
  | Bool_tok i1, Int_tok i2 -> Int_tok (int_of_bool(i1) / i2)
  | Int_tok i1, Bool_tok i2 -> Int_tok (i1 / int_of_bool(i2))
  | Bool_tok i1, Bool_tok i2 -> Int_tok (int_of_bool(i1) / int_of_bool(i2))
  | Bool_tok i1, Float_tok i2 -> Float_tok (float_of_bool(i1) /. i2)
  | Float_tok i1, Bool_tok i2 -> Float_tok (i1 /. float_of_bool(i2))
  | Id_tok i1, _ -> eval_div (get_val expr1 symtable) expr2 symtable
  | _, Id_tok i2 -> eval_div expr1 (get_val expr2 symtable) symtable 
  | _, _ -> raise IgnoreCase
;;




