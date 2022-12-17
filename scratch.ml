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


      letrec ((is-even? (lambda (n)
                        (or (zero? n)
                            (is-odd? (sub1 n)))))
            (is-odd? (lambda (n)
                        (and (not (zero? n))
                            (is-even? (sub1 n))))))



  (define x 1)   free
  (lambda () (+ x 2))
  (lambda (x 2) (+ x 3))

  (let ((x 3)) (+ x 4)) ->
    ((lambda (x) + x 4)3)
    
    
    lambda(w x)
      lamnda(y) 
        lambda (z) + x y z



   (if (> 3 5)
      (+ 3 10)
      (+ 5 10))

  annotate_lexical_address iftest;;
    - : expr' =
    ScmIf'
    (ScmApplic' (ScmVarGet' (Var' (">", Free)),
      [ScmConst' (ScmNumber (ScmRational (3, 1)));
        ScmConst' (ScmNumber (ScmRational (5, 1)))],
      Non_Tail_Call),
    ScmApplic' (ScmVarGet' (Var' ("+", Free)),
      [ScmConst' (ScmNumber (ScmRational (3, 1)));
      ScmConst' (ScmNumber (ScmRational (10, 1)))],
      Non_Tail_Call),
    ScmApplic' (ScmVarGet' (Var' ("+", Free)),
      [ScmConst' (ScmNumber (ScmRational (5, 1)));
      ScmConst' (ScmNumber (ScmRational (10, 1)))],
      Non_Tail_Call))




    (if (> 3 x)
      (+ 3 10)
      (+ x 10))

    - : expr' =
    ScmIf'
    (ScmApplic' (ScmVarGet' (Var' (">", Free)),
      [ScmConst' (ScmNumber (ScmRational (3, 1)));
        ScmVarGet' (Var' ("x", Free))],
      Non_Tail_Call),
    ScmApplic' (ScmVarGet' (Var' ("+", Free)),
      [ScmConst' (ScmNumber (ScmRational (3, 1)));
      ScmConst' (ScmNumber (ScmRational (10, 1)))],
      Non_Tail_Call),
    ScmApplic' (ScmVarGet' (Var' ("+", Free)),
      [ScmVarGet' (Var' ("x", Free));
      ScmConst' (ScmNumber (ScmRational (10, 1)))],
      Non_Tail_Call))


      (lambda(x)
        (if x 3 5))

      ((lambda(x)
        (if x 3 5)) 9) 

        val iflamtest : expr' =
          ScmApplic'
          (ScmLambda' (["x"], Simple,
            ScmIf' (ScmVarGet' (Var' ("x", Param 0)),
              ScmConst' (ScmNumber (ScmRational (3, 1))),
              ScmConst' (ScmNumber (ScmRational (5, 1))))),
          [ScmConst' (ScmNumber (ScmRational (9, 1)))],
          Non_Tail_Call)

((lambda (y)
  ((lambda(x)
        (if x 3 5)) 9) )13)
          
val iflamtest : expr' =
  ScmApplic'
   (ScmLambda' (["y"], Simple,
     ScmApplic'
      (ScmLambda' (["x"], Simple,
        ScmIf' (ScmVarGet' (Var' ("x", Param 0)),
         ScmConst' (ScmNumber (ScmRational (3, 1))),
         ScmConst' (ScmNumber (ScmRational (5, 1))))),
      [ScmConst' (ScmNumber (ScmRational (9, 1)))],
      Non_Tail_Call)),
   [ScmConst' (ScmNumber (ScmRational (13, 1)))],
   Non_Tail_Call)


  ((lambda (y)
  ((lambda(x)
        (if x y 5)) 9) )13)

    val iflamtest : expr' =
      ScmApplic'
      (ScmLambda' (["y"], Simple,
        ScmApplic'
          (ScmLambda' (["x"], Simple,
            ScmIf' (ScmVarGet' (Var' ("x", Param 0)),
            ScmVarGet' (Var' ("y", Bound (0, 0))),
            ScmConst' (ScmNumber (ScmRational (5, 1))))),
          [ScmConst' (ScmNumber (ScmRational (9, 1)))],
          Non_Tail_Call)),
      [ScmConst' (ScmNumber (ScmRational (13, 1)))],
      Non_Tail_Call)

      (define x 5)

      (define x 6)


(* | ScmSeq' of expr' list *)

| ScmSeq(exprs) -> ScmSeq'(map3 run exprs params env)


(* | ScmOr(exprs) -> ScmOr'(map3 run exprs params env) *)

let rec map3 f ls first second =
    match ls with
    | [] -> []
    | h :: t -> (f h first second) :: (map3 f t first second);;


    let t = "(lambda (x) (begin (set! x 1) (set! x 2) (set! x 3)))";;
    let test = annotate_lexical_address(tag_parse((nt_sexpr t 0).found));;


    if x
      y...
      if z
        w
        r

        tailcall = false



    let rec run in_tail = function
      | ScmSeq' [] -> raise X_not_yet_implemented
      | ScmSeq' (expr :: exprs) -> X_not_yet_implemented



(* *** *)

  let annotate_tail_calls = 
    let rec run in_tail = function
      | (ScmConst' _) as orig -> orig
      | (ScmVarGet' _) as orig -> orig
      | ScmIf' (test, dit, dif) -> ScmIf'(run false test, run in_tail dit, run in_tail dif)

      | ScmSeq' [] as orig -> orig
      | ScmSeq' (expr :: exprs) -> runl in_tail expr exprs

      | ScmOr' [] as orig -> orig
      | ScmOr' (expr :: exprs) -> runl in_tail expr exprs
      | ScmVarSet' (var', expr') -> ScmVarSet'(var', run false expr')
      | ScmVarDef' (var', expr') -> ScmVarDef'(var', run false expr')
      | (ScmBox' _) as expr' -> raise X_not_yet_implemented
      | (ScmBoxGet' _) as expr' -> raise X_not_yet_implemented
      | ScmBoxSet' (var', expr') -> raise X_not_yet_implemented
      | ScmLambda' (params, Simple, expr) -> raise X_not_yet_implemented
      | ScmLambda' (params, Opt opt, expr) -> raise X_not_yet_implemented
       | ScmApplic' (proc, args, app_kind) ->
         if in_tail
         then ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Tail_Call)
         else ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Non_Tail_Call)
    and runl in_tail expr = function
      | [] -> [run in_tail expr]
      | expr' :: exprs -> (run false expr) :: (runl in_tail expr' exprs)
    in
    fun expr' -> raise X_not_yet_implemented;;














    let annotate_tail_calls = 
    let rec run in_tail = function
      | (ScmConst' _) as orig -> raise X_not_yet_implemented
      | (ScmVarGet' _) as orig -> raise X_not_yet_implemented
      | ScmIf' (test, dit, dif) -> raise X_not_yet_implemented
      | ScmSeq' [] -> raise X_not_yet_implemented
      | ScmSeq' (expr :: exprs) -> raise X_not_yet_implemented
      | ScmOr' [] -> raise X_not_yet_implemented
      | ScmOr' (expr :: exprs) -> raise X_not_yet_implemented
      | ScmVarSet' (var', expr') -> raise X_not_yet_implemented
      | ScmVarDef' (var', expr') -> raise X_not_yet_implemented
      | (ScmBox' _) as expr' -> raise X_not_yet_implemented
      | (ScmBoxGet' _) as expr' -> raise X_not_yet_implemented
      | ScmBoxSet' (var', expr') -> raise X_not_yet_implemented
      | ScmLambda' (params, Simple, expr) -> raise X_not_yet_implemented
      | ScmLambda' (params, Opt opt, expr) -> raise X_not_yet_implemented
      | ScmApplic' (proc, args, app_kind) ->
         if in_tail
         then ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Tail_Call)
         else ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Non_Tail_Call)
    and runl in_tail expr = function
      | [] -> [run in_tail expr]
      | expr' :: exprs -> (run false expr) :: (runl in_tail expr' exprs)
    in
    fun expr' -> raise X_not_yet_implemented;;


type expr =
  | ScmConst of sexpr
  | ScmVarGet of var
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmOr of expr list
  | ScmVarSet of var * expr
  | ScmVarDef of var * expr
  | ScmLambda of string list * lambda_kind * expr
  | ScmApplic of expr * expr list;;

  type app_kind = Tail_Call | Non_Tail_Call;;

  type lexical_address =
    | Free
    | Param of int
    | Bound of int * int;;

  type var' = Var' of string * lexical_address;;


type expr' =
  | ScmConst' of sexpr
  | ScmVarGet' of var'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmOr' of expr' list
  | ScmVarSet' of var' * expr'
  | ScmVarDef' of var' * expr'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmLambda' of string list * lambda_kind * expr'
  | ScmApplic' of expr' * expr' list * app_kind;;



let annotate_tail_calls = 
   let rec run in_tail = function
     | (ScmConst' _) as orig -> orig
     | (ScmVarGet' _) as orig -> orig
     | ScmIf' (test, dit, dif) -> ScmIf'(run false test, run in_tail dit, run in_tail dif)

     | ScmSeq' [] as orig -> orig
     | ScmSeq' (expr :: exprs) -> runl in_tail expr exprs

     | ScmOr' []as orig -> orig
     | ScmOr' (expr :: exprs) -> runl in_tail expr exprs
     
     | ScmVarSet' (var', expr') -> ScmVarSet'(var', run false expr')
     | ScmVarDef' (var', expr') -> ScmVarDef'(var', run false expr')
     | (ScmBox' _) as expr' -> raise X_not_yet_implemented
     | (ScmBoxGet' _) as expr' -> raise X_not_yet_implemented
     | ScmBoxSet' (var', expr') -> raise X_not_yet_implemented
     | ScmLambda' (params, Simple, expr) -> ScmLambda'(params, Simple, run true expr)
     | ScmLambda' (params, Opt opt, expr) -> ScmLambda'(params, Opt opt, run true expr)
     | ScmApplic' (proc, args, app_kind) ->
        if in_tail
        then ScmApplic' (run false proc,
                         List.map (fun arg -> run false arg) args,
                         Tail_Call)
        else ScmApplic' (run false proc,
                         List.map (fun arg -> run false arg) args,
                         Non_Tail_Call)
   and runl in_tail expr = function
     | [] -> [run in_tail expr]
     | expr' :: exprs -> (run false expr) :: (runl in_tail expr' exprs)
   in
   fun expr' -> raise X_not_yet_implemented;;




   let t = "(if (> x 3) x 3)";;
    let test = annotate_lexical_address(tag_parse((nt_sexpr t 0).found));;

  let show str = Printf.printf "\n\n%a\n\n"
  print_expr (tag_parse
  (nt_sexpr str 0).found);;

  let show str = annotate_tail_calls(
         annotate_lexical_address expr(
          (tag_parse ((nt_sexpr str 0).found))));;