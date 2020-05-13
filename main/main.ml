#use "lexer.mml.ml";;

let lteq tok1 tok2 = match tok1, tok2 with
    Plus_tok, Plus_tok -> true
  | Plus_tok, Minus_tok -> true
  | Minus_tok, Plus_tok -> true
  | Times_tok, Plus_tok -> false
  | Plus_tok, Times_tok -> true
  | Times_tok, Minus_tok -> false
  | Minus_tok, Times_tok -> true
  | Div_tok, Plus_tok -> false
  | Plus_tok, Div_tok -> true
  | Div_tok, Minus_tok -> false
  | Minus_tok, Div_tok -> true
  | Times_tok, Div_tok -> true
  | Div_tok, Times_tok -> true
  | Times_tok, Times_tok -> true
  | Div_tok, Div_tok -> true
  | Minus_tok, Minus_tok -> true
  | _, _ -> raise IgnoreCase
;;

let head list = match list with
    [] -> raise IgnoreCase
  | x::xs -> x
;;

let evaluate_op op v1 v2 symtable = match op with
    Plus_tok -> eval_plus v1 v2 symtable
  | Minus_tok -> eval_minus v1 v2 symtable
  | Times_tok -> eval_times v1 v2 symtable
  | Div_tok -> eval_div v1 v2 symtable
  | _ -> raise IgnoreCase
;;


let evaluate tokens symtable =
  let rec eval tokens vallist oplist =
    match tokens with
      [] -> ( match oplist with
                [] -> head vallist
              |  _ -> let rec final_evaluate vallist oplist = match oplist with
                          [] -> ( match vallist with
                                    [] -> raise IgnoreCase
                                  |  _ -> head vallist
                                )
                        | k::ks -> ( match vallist with
                                       v1::v2::v -> final_evaluate ((evaluate_op k v2 v1 symtable)::v) ks
                                     | _ -> raise IgnoreCase
                                   )
                      in
                      final_evaluate vallist oplist
                                          
            )
    |  x::xs -> ( match x with
                    Int_tok i -> eval xs (x::vallist) oplist
                  | Float_tok f -> eval xs (x::vallist) oplist
                  | Bool_tok b -> eval xs (x::vallist) oplist
                  | Id_tok id -> eval xs ((get_val x symtable)::vallist) oplist
                  | _ -> ( match oplist with
                                    [] -> eval xs vallist (x::[])
                                  | _ -> let rec preeval vallist oplist ctok = ( match oplist with
                                                                               [] -> eval xs vallist (ctok::oplist)
                                                                             | p::ps -> if lteq ctok p then
                                                                                          ( match vallist with
                                                                                              v1::v2::v -> preeval ((evaluate_op p v2 v1 symtable)::v) ps ctok
                                                                                            | _ -> raise IgnoreCase
                                                                                          )
                                                                                        else
                                                                                          eval xs vallist (ctok::oplist)
                                                                           )
                                         in preeval vallist oplist x
                         )
                )
              
  in
  eval tokens [] []
;;


let eval_eq id1 exp symtable = match id1 with
    Id_tok id -> ((id1, (evaluate exp symtable))::symtable)
  | _ -> raise IgnoreCase
;;

let evaluate_s expr symtable = match expr with
    id1::(Equ_tok)::exp -> eval_eq id1 exp symtable
  | _ -> let _ = print_token (evaluate expr symtable) in
         let _ = print_string "\n" in
         symtable
;;

let print_symtable symtable =
  let _ = print_string "[" in
  let rec print symtable = match symtable with
      [] -> ()
    | x::xs -> let _ = print_string "(" in
               let _ = ( match x with
                           t1, t2 -> let _ = print_token t1 in
                                     let _ = print_string ", " in
                                     let _ = print_token t2 in
                                     print_string ")"
                       )
               in
               print xs
  in
  let _ = print symtable in
  print_string "]"
;;

let rec run_interpreter symtable =
  let _ = print_string ">>> " in
  let input = read_line() in
  if input = "exit()" then
    ()
  else
    let tokens = tokenize input in
    match tokens with
      [] -> run_interpreter symtable
    | _ ->
       let symtable = (evaluate_s tokens symtable) in
       run_interpreter symtable
;;

run_interpreter [];;
