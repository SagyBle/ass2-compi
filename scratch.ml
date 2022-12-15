| ScmPair 
 (ScmSymbol "let*",
  ScmPair 
   (ScmPair 
     (ScmPair
       (var,
        ScmPair (arg, ScmNil)),

      ribs),

   exprs))


(* now *)

ScmPair
 (ScmSymbol "let*",
  ScmPair
   (ScmPair
     (ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),
      ScmPair
       (ScmPair
         (ScmSymbol "y", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
        ScmNil)),
    ScmPair
     (ScmPair
       (ScmSymbol "+",
        ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil))),
      ScmNil)))










ScmPair
 (ScmSymbol "let",
  ScmPair
   (ScmPair
     (ScmPair
       (var,
        ScmPair (arg, ScmNil)),
      ScmNil),
    ScmPair
     (ScmPair
       (ScmSymbol "let*",
        ScmPair
         (ribs,
          exprs)),
      ScmNil)))



ScmPair
 (ScmSymbol "let",
  ScmPair
   (ScmPair
     (ScmPair
       (var,
        ScmPair (arg, ScmNil)),
      ScmNil),
    ScmPair
     (ScmPair
       ((ScmSymbol "let*",
        ScmPair
         (ribs,
          exprs))),
      ScmNil)))







(* wanted 16:24 *)

ScmPair
 (ScmSymbol "let",
  ScmPair
   (ScmPair
     (ScmPair (var, ScmPair (arg, ScmNil)),
      ScmNil),
    ScmPair
     (ScmPair
       (ScmSymbol "let",
        ScmPair
         (ribs,
          exprs)),
      ScmNil)))



ScmPair
  (ScmSymbol "let",
  ScmPair
    (ScmPair
    (ScmPair (var, ScmPair (arg, ScmNil)),
        ScmNil),
    ScmPair
    (ScmPair
        macro_expand(ScmPair(ribs, exprs)),
        ScmNil)))


  let macro_expand = function
  | ScmPair(ribs,exprs) -> 
  | ScmPair(ScmNil, exprs) ->


ScmPair
 (ScmSymbol "let",
  ScmPair
   (ScmPair
     (ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
      ScmNil),
    ScmPair
     (ScmPair
       (ScmSymbol "let*",
        ScmPair
         (ScmPair
           (ScmPair
             (ScmSymbol "z", ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil)),
            ScmNil),
          ScmPair
           (ScmPair
             (ScmSymbol "+",
              ScmPair (ScmSymbol "z", ScmPair (ScmSymbol "y", ScmNil))),
            ScmNil))),
      ScmNil)))






















(* modified *)

ScmPair
 (ScmSymbol "let",
  ScmPair
   (ScmPair
     (ScmPair (var, ScmPair (arg, ScmNil)),
      ScmNil),
    ScmPair
     (ScmPair
       (ScmSymbol "let*",
        ScmPair
         (ScmPair
           (ScmPair
             (ScmSymbol "y", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
            ScmNil),
          exprs)),
      ScmNil)))
  










let rec tag_parse2_var sexpr =
    match sexpr with
| ScmPair (ScmSymbol "let*",
               ScmPair (ScmPair (ScmPair (var,
                                          ScmPair (arg, ScmNil)),
                                 ribs),
                        exprs)) -> var ;;
let rec tag_parse2_arg sexpr =
    match sexpr with
| ScmPair (ScmSymbol "let*",
               ScmPair (ScmPair (ScmPair (var,
                                          ScmPair (arg, ScmNil)),
                                 ribs),
                        exprs)) -> arg ;;
let rec tag_parse2_ribs sexpr =
    match sexpr with
| ScmPair (ScmSymbol "let*",
               ScmPair (ScmPair (ScmPair (var,
                                          ScmPair (arg, ScmNil)),
                                 ribs),
                        exprs)) -> ribs ;;

let rec tag_parse2_exprs sexpr =
    match sexpr with
| ScmPair (ScmSymbol "let*",
               ScmPair (ScmPair (ScmPair (var,
                                          ScmPair (arg, ScmNil)),
                                 ribs),
                        exprs)) -> exprs ;;






























| ScmPair (ScmSymbol "let", ScmPair (ribs, exprs)) ->
         let ribs = scheme_list_to_ocaml ribs in
         
         let vars = List.map
            (fun var -> (match val with
            | ScmPair(var, expr) -> var
            | _ -> raise (X_syntax ("rib in let is not good")))) ribs in

         let vals = List.map
            (fun val -> (match val with
            | ScmPair(var, ScmPair(expr, ScmNil)) -> expr
            | _ -> raise (X_syntax ("rib in let is not good")))) ribs in

         ScmPair(ScmPair(ScmSymbol("lambda"), ScmPair(vars, exprs)), vals)
 
  (let 
    ((a 5)
     (b 2))
    (+ a b)) 
                
  (let 
    ((a 5)
     (b 2)
     (c 3))
    (- (+ a b) c)) 

let rec list_to_proper_list = function
  | [] -> ScmNil
  | hd::[] -> ScmPair (hd, ScmNil)
  | hd::tl -> ScmPair (hd, list_to_proper_list tl);;


| ScmPair (ScmSymbol "let", ScmPair (ribs, exprs)) ->
         let ribs = scheme_list_to_ocaml ribs in
         
         let vars = List.map
            (fun x -> (match x with
            | ScmPair(var, _) -> var
            | _ -> raise (X_syntax ("rib in let is not good")))) ribs in

         let vals = List.map
            (fun x -> (match x with
            | ScmPair(var, ScmPair(expr, ScmNil)) -> expr
            | _ -> raise (X_syntax ("rib in let is not good")))) ribs in

         ScmPair(ScmPair(ScmSymbol("lambda"), ScmPair(vars, exprs)), vals)


ribs:

    ScmPair
     (ScmPair
      (* rib1 *)
       (ScmPair
         (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (5, 1)), ScmNil)),
        ScmPair
      (* rib 2 *)
         (ScmPair
           (ScmSymbol "b", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
          ScmNil)




  (let 
    ((a 5)
     (b 2))
    (+ a b)) 

(* ribs *)
ScmPair
 (ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (5, 1)), ScmNil)),
  ScmPair
   (ScmPair (ScmSymbol "b", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
    ScmNil))

(* exprs *)
ScmPair
 (ScmPair
   (ScmSymbol "+", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))),
  ScmNil)



  | ScmPair (ScmSymbol "let", ScmPair (ribs, exprs)) ->

    let no_nil_func = fun (x, ScmNil) -> x;;

    let ribs_list_nil = scheme_list_to_ocaml ribs;;  (* this one with nil in the end that fucks us *)
    let ribs_list = no_nil_func ribs_list_nil ;;

    (* let exprs_list_nil = scheme_list_to_ocaml exprs;;  (* this one with nil in the end that fucks us *)
    let exprs_list = no_nil_func exprs_list_nil;; *)
    let exprs_list = exprs;;


    (* ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (5, 1)), ScmNil)) *)

    (* var may be problematic:this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
ScmPair
  (ScmSymbol _,
  ScmPair
    (_,
    (ScmVoid|ScmBoolean _|ScmChar _|ScmString _|ScmSymbol _|ScmNumber _|
    ScmVector _|ScmPair (_, _))))
 *)
    let get_var = fun (ScmPair( symbol_var, ScmPair(value, ScmNil))) -> symbol_var;;
    let get_value = fun (ScmPair(symbol_var, ScmPair(value, ScmNil))) -> value;;
    
    let vars_symbols_list = List.map get_var ribs_list;;
    let vals_list = List.map get_value ribs_list;;
    let exprs_list_body = exprs_list

    
    ((lambda (vars) body)
    values)

    (* ScmPair(
      ScmPair(ScmSymbol("lambda"),
        ScmPair(list_to_proper_list vars_symbols_list,
          ScmPair(list_to_proper_list exprs_list_body, ScmNil))), 
    ScmPair(list_to_proper_list vals_list, ScmNil)) *)

    (* ScmPair(
      ScmPair(ScmSymbol("lambda"),
        ScmPair(list_to_proper_list vars_symbols_list,
          ScmPair(list_to_proper_list exprs_list_body, ScmNil))), 
    list_to_proper_list vals_list) *)

    ScmPair(
      ScmPair(ScmSymbol("lambda"),
        ScmPair(list_to_proper_list vars_symbols_list,
          list_to_proper_list exprs_list_body)), 
    list_to_proper_list vals_list)







    (* recicle been 17:40 *)
    (* let ribs = scm_list_to_list ribs in

         
         
         let vars = List.map
            (fun x -> (match x with
            | ScmPair(var, _) -> var
            | _ -> raise (X_syntax ("rib in let is not good")))) ribs in

         let vals = List.map
            (fun x -> (match x with
            | ScmPair(var, ScmPair(expr, ScmNil)) -> expr
            | _ -> raise (X_syntax ("rib in let is not good")))) ribs in

         tag_parse (ScmPair(ScmPair(ScmSymbol("lambda"), ScmPair(vars, exprs))), (sexpr_of_exprvals) *)





  let rec tag_parse5 sexpr =
    match sexpr with
    | ScmPair (ScmSymbol "let*",
          ScmPair (ScmPair (ScmPair (var,
                                      ScmPair (arg, ScmNil)),
                            ribs),
                    exprs)) -> ScmSymbol("ok");;




tag_parse (ScmPair
         (ScmSymbol "let",
         ScmPair
            (ScmPair
            (ScmPair (var, ScmPair (arg, ScmNil)),
               ScmNil),
            ScmPair
            macro_expand(ribs,exprs))))


ScmPair(ScmSymbol("let*"),ScmPair(ribs,body)) ->
      (match ribs with
      | ScmNil | ScmPair(_,ScmNil) -> macro_expand (ScmPair(ScmSymbol("let"),ScmPair(ribs,body)))
      | ScmPair(exp1, rest) -> 
          macro_expand (ScmPair(
                        ScmSymbol("let"),
                        ScmPair(
                          ScmPair(exp1,ScmNil),
                          ScmPair[
                            ScmPair(
                              ScmSymbol("let*"),
                              ScmPair(rest,body)),
                            ScmNil))))
      | _ -> raise (X_syntax_error(sexpr,"Bad syntax for let*"))






    (* Shay function *)

    
  