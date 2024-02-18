(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

type dispatch =
  { parse_un_op : dispatch -> expr Angstrom.t
  ; parse_bin_op : dispatch -> expr Angstrom.t
  ; parse_list : dispatch -> expr Angstrom.t
  ; parse_list_cons : dispatch -> expr Angstrom.t
  ; parse_tuple : dispatch -> expr Angstrom.t
  ; parse_application : dispatch -> expr Angstrom.t
  ; parse_fun : dispatch -> expr Angstrom.t
  ; parse_match_with : dispatch -> expr Angstrom.t
  ; parse_if_then_else : dispatch -> expr Angstrom.t
  ; parse_effect_with_arguments : dispatch -> expr Angstrom.t
  ; parse_perform : dispatch -> expr Angstrom.t
  ; parse_continue : dispatch -> expr Angstrom.t
  }

(* Constructors for expressions *)
let econst x = EConst x
let ebinop op left_op right_op = EBinaryOperation (op, left_op, right_op)
let eunop operator operand = EUnaryOperation (operator, operand)
let elist cont = EList cont
let elistcons l r = EListCons (l, r)
let etuple cont = ETuple cont
let eidentifier x = EIdentifier x
let eapplication f x = EApplication (f, x)
let efun var expression = EFun (var, expression)
let declraration func_name expression exp_in = EDeclaration (func_name, expression, exp_in)

let rec_declraration func_name expression exp_in =
  ERecDeclaration (func_name, expression, exp_in)
;;

let eif_then_else condition true_b false_b = EIfThenElse (condition, true_b, false_b)
let ematch_with expression cases = EMatchWith (expression, cases)

let eeffect_without_arguments name = EEffectWithoutArguments name
let eefect_with_arguments name arg = EEffectWithArguments (name, arg)
let eeffect_perform expr = EEffectPerform expr
let eeffect_continue cont expr = EEffectContinue (cont, expr)

let aint = AInt
let abool = ABool
let achar = AChar
let astring = AString
let aunit = AUnit
let aarrow l r = AArrow (l, r)
let alist a = AList a
let atuple alist = ATuple alist
let aeffect a = AEffect a
(* ---------------- *)

(* Constructors for binary operations *)
let sAdd _ = Add
let sSub _ = Sub
let sMul _ = Mul
let sDiv _ = Div
let sEq _ = Eq
let sNEq _ = NEq
let sGt _ = Gt
let sGte _ = Gte
let sLt _ = Lt
let sLte _ = Lte
let sAnd _ = And
let sOr _ = Or
(* ---------------- *)

(* Constructors for unary operations *)
let uMin _ = Minus
let uNot _ = Not
let uPlus _ = Plus
(* ---------------- *)

(* Constructors for patterns *)
let pAny _ = PAny
let pNill _ = PNill
let pConst c = PConst c
let pVal v = PVal v
let pListCons l r = PListCons (l, r)
let pTuple l = PTuple l
(* ---------------- *)

(* Constructors for patterns *)
let aArrow a1 a2 = AArrow (a1, a2)
let aTuple l = ATuple l
let aList a = AList a
let aEffect a = AEffect a

(* ----------------- *)
let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "continue"
  | "do"
  | "done"
  | "downto"
  | "effect"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "perform"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_wspace = skip_while is_whitespace
let skip_wspace1 = take_while1 is_whitespace

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_ident c = is_lower c || is_upper c || c = '_'

let is_acceptable_fl = function
  | Some c when is_lower c || c = '_' -> return c
  | _ -> fail "abc"
;;

let rec chainr1 e op =
  e >>= fun se -> op >>= (fun f -> chainr1 e op >>| f se) <|> return se
;;

let is_letter c = is_upper c || is_lower c
let parens p = skip_wspace *> char '(' *> p <* skip_wspace <* char ')'
let sqr_parens p = skip_wspace *> char '[' *> p <* skip_wspace <* char ']'
let braces p = skip_wspace *> char '{' *> p <* skip_wspace <* char '}'
let list_constr = skip_wspace *> string "::"

let list_sep =
  skip_wspace *> char ';' *> skip_wspace
  <|> (skip_wspace *> peek_char
       >>= function
       | Some c when c != ']' -> fail "Error: Expected semicolon or end of list"
       | _ -> return ())
;;

let parse_name =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|> take_while1 (fun x ->
        is_lower x || is_upper x || is_digit x || x = '\'' || x = '_'))
;;

let parse_uncapitalized_name =
  parse_name
  >>= fun name ->
  if (is_lower name.[0] || name.[0] = '_') && is_keyword name != true
  then return name
  else fail "Parsing error: not an uncapitalized entity."
;;

let parse_capitalized_name =
  parse_name
  >>= fun name ->
  if (is_upper name.[0]) && is_keyword name != true
  then return name
  else fail "Parsing error: not an capitalized entity."
;;

let parse_cint = take_while1 is_digit >>| int_of_string >>| fun x -> Int x
let parse_cstring = char '"' *> take_while (( != ) '"') <* char '"' >>| fun x -> String x
let parse_cchar = char '\'' *> any_char <* char '\'' >>| fun x -> Char x
let parse_cbool = string "true" <|> string "false" >>| bool_of_string >>| fun x -> Bool x
let parse_cunit = string "()" >>| fun _ -> Unit

let const constr =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let const =
        choice [ parse_cint; parse_cstring; parse_cchar; parse_cbool; parse_cunit ]
      in
      lift constr const)
;;

let ident constr =
  skip_wspace *> peek_char
  >>= is_acceptable_fl
  >>= fun _ ->
  take_while is_ident
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: name is used as keyword"
  else return @@ constr s
;;

(*Patterns parsers*)
let parse_pattern_nill = sqr_parens skip_wspace >>| pNill
let parse_pattern_any = skip_wspace *> char '_' >>| pAny
let parse_pattern_val = ident pVal
let parse_pattern_const = const pConst
let parse_pattern_list_constr = list_constr *> return pListCons

let parse_tuple p_pattern =
  parens
  @@ lift2
       (fun h tl -> pTuple @@ (h :: tl))
       p_pattern
       (many1 (skip_wspace *> string "," *> p_pattern))
;;

let parse_primitive_pattern =
  choice [ parse_pattern_nill; parse_pattern_any; parse_pattern_val; parse_pattern_const ]
;;

let parse_pattern =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_pattern_list_constr =
    chainr1 (parens self <|> parse_primitive_pattern) parse_pattern_list_constr
  in
  choice
    [ parse_pattern_list_constr; parse_primitive_pattern; parse_tuple self; parens self ]
;;

(* ---------------- *)

(* Types annotation parsers *)
let parse_prim_type =
  choice
    [ skip_wspace *> string "int" *> return AInt
    ; skip_wspace *> string "bool" *> return ABool
    ; skip_wspace *> string "char" *> return AChar
    ; skip_wspace *> string "string" *> return AString
    ; skip_wspace *> string "unit" *> return AUnit
    ]
;;

let parse_arrow p_type = lift2 aArrow (p_type <* skip_wspace <* string "->") p_type

let parse_tuple_type p_type =
  lift2
    (fun h tl -> aTuple @@ (h :: tl))
    p_type
    (many1 (skip_wspace *> string "*" *> p_type))
;;

let parse_list_type p_type = lift aList (p_type <* skip_wspace <* string "list")
let parse_effect_type p_type = lift aEffect (p_type <* skip_wspace <* string "effect")

let parse_type_annotation =
  fix
  @@ fun self ->
  skip_wspace
  *> choice
       [ parse_prim_type
       ; parse_arrow self
       ; parse_tuple_type self
       ; parse_list_type self
       ; parse_effect_type self
       ; parens self
       ]
;;

(* ---------------- *)

(* Expressions parsers *)
let parse_ident = ident eidentifier
let parse_const = const econst

let parse_effect_without_arguments =
  fix
  @@ fun self ->
  skip_wspace 
  *> (parens self <|>
      lift
        eeffect_without_arguments
        parse_capitalized_name
  )
;;

let parse_effect_with_arguments pack =
  fix
  @@ fun self ->
  let parse_expr = 
    choice [ parens @@ pack.parse_tuple pack
    ; parens @@ pack.parse_list_cons pack
    ; parens @@ pack.parse_bin_op pack
    ; parens @@ pack.parse_un_op pack
    ; pack.parse_list pack
    (* ; parens @@ pack.parse_perform pack *)
    ; parens @@ pack.parse_application pack
    ; parens @@ pack.parse_fun pack
    ; parens @@ pack.parse_if_then_else pack
    ; parens @@ pack.parse_match_with pack
    ; parse_const
    ; parse_ident ]
  in
  skip_wspace
  *> (parens self <|>
      lift2
        eefect_with_arguments
        parse_capitalized_name
        parse_expr)
;;

let parse_perform pack =
  fix
  @@ fun self ->
    let perform = skip_wspace *> string "perform" <* skip_wspace in
  skip_wspace *>
  lift
    eeffect_perform
    (perform *> (parse_effect_with_arguments pack <|> parse_effect_without_arguments))
;;

let parse_continue pack =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
     <|>
     let parse_expr =
       choice
         [ parens @@ pack.parse_tuple pack
         ; parens @@ pack.parse_list_cons pack
         ; parens @@ pack.parse_bin_op pack
         ; parens @@ pack.parse_un_op pack
         ; pack.parse_list pack
         ; parens @@ pack.parse_perform pack
         ; parens @@ pack.parse_application pack
         ; parens @@ pack.parse_fun pack
         ; parens @@ pack.parse_if_then_else pack
         ; parens @@ pack.parse_match_with pack
         ; parse_const
         ; parse_ident
         ]
     in
     let parse_continue = skip_wspace *> string "continue" *> skip_wspace in
     lift2 eeffect_continue (parse_continue *> parse_uncapitalized_name) (skip_wspace *> parse_expr))
;;

let parse_fun pack =
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_match_with pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  skip_wspace *> string "fun" *> many1 parse_pattern
  >>= fun args ->
  skip_wspace *> string "->" *> parse_expr
  >>= fun expr ->
  match List.rev args with
  | h :: tl -> return (List.fold_left (fun acc x -> efun x acc) (efun h expr) tl)
  | _ -> fail "Error"
;;

let parse_if_then_else pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_match_with pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; self
      ; parse_const
      ; parse_ident
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           eif_then_else
           parse_expr
           (skip_wspace *> string "then" *> parse_expr)
           (skip_wspace *> string "else" *> parse_expr)
;;

let parse_bin_op pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let addition = skip_wspace *> char '+' >>| sAdd
  and subtraction = skip_wspace *> char '-' >>| sSub
  and multiplication = skip_wspace *> char '*' >>| sMul
  and division = skip_wspace *> char '/' >>| sDiv
  and eqality = skip_wspace *> char '=' >>| sEq
  and neqality = skip_wspace *> string "!=" <|> string "<>" >>| sEq
  and logand = skip_wspace *> string "&&" >>| sAnd
  and logor = skip_wspace *> string "||" >>| sOr
  and larger = skip_wspace *> char '>' >>| sGt
  and largerEq = skip_wspace *> string ">=" >>| sGte
  and less = skip_wspace *> char '<' >>| sLt
  and lessEq = skip_wspace *> string "<=" >>| sLte in
  let parse_expr =
    choice
      [ parens self
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_match_with pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  and chainl1 e op =
    let rec go acc =
      lift2 (fun f x -> EBinaryOperation (f, acc, x)) op e >>= go <|> return acc
    in
    e >>= fun init -> go init
  in
  List.fold_left
    (fun acc x -> chainl1 acc x)
    (chainl1 parse_expr multiplication)
    [ division
    ; addition
    ; subtraction
    ; larger
    ; largerEq
    ; less
    ; lessEq
    ; eqality
    ; neqality
    ; logand
    ; logor
    ]
  >>= fun res ->
  match res with
  | EBinaryOperation (_, _, _) -> return res
  | _ -> fail "Error: not binary operation."
;;

let parse_un_op pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_minus = skip_wspace *> char '-' >>| uMin
  and parse_plus = skip_wspace *> char '+' >>| uPlus
  and parse_not = skip_wspace *> string "not" >>| uNot
  and parse_content_minus_and_plus =
    choice
      [ parens self
      ; parens @@ pack.parse_application pack
      ; parens @@ pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ; parens @@ pack.parse_bin_op pack
      ]
  and parse_content_not =
    choice
      [ parens @@ pack.parse_bin_op pack
      ; parens self
      ; parens @@ pack.parse_application pack
      ; parens @@ pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  parens self
  <|> lift2 eunop parse_minus parse_content_minus_and_plus
  <|> lift2 eunop parse_plus parse_content_minus_and_plus
  <|> lift2 eunop parse_not parse_content_not
;;

let parse_list pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_match_with pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let content = skip_wspace *> many (parse_expr <* list_sep) in
  parens self <|> lift elist @@ sqr_parens @@ content
;;

let parse_list_cons pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ parens @@ pack.parse_tuple pack
      ; parens self
      ; parens @@ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_application pack
      ; parens @@ pack.parse_fun pack
      ; parens @@ pack.parse_if_then_else pack
      ; parens @@ pack.parse_match_with pack
      ; parse_const
      ; parse_ident
      ]
  in
  let left = parse_expr <* list_constr in
  let right = skip_wspace *> (self <|> parse_expr) in
  lift2 elistcons left right
;;

let parse_tuple pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_match_with pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  parens
  @@ lift2
       (fun h tl -> etuple @@ (h :: tl))
       parse_expr
       (many1 (skip_wspace *> string "," *> parse_expr))
;;

let parse_match_with pack =
  fix
  @@ fun self ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let parse_case =
    lift2
      (fun pat expr -> pat, expr)
      (skip_wspace *> string "|" *> parse_pattern)
      (skip_wspace *> string "->" *> parse_expr)
  in
  skip_wspace
  *> string "match"
  *> lift2 ematch_with (parse_expr <* skip_wspace <* string "with") (many1 parse_case)
;;

let parse_declaration pack =
  fix
  @@ fun self ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_match_with pack
      ; self
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let helper constr =
    lift3
      constr
      parse_uncapitalized_name
      (many parse_pattern
       >>= fun args ->
       skip_wspace *> string "=" *> parse_expr
       >>= fun expr ->
       skip_wspace
       *>
       match List.rev args with
       | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
       | _ -> return expr)
      (skip_wspace *> string "in" *> parse_expr >>| (fun e -> Some e) <|> return None)
  in
  skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  >>= function
  | "rec" -> helper rec_declraration
  | _ -> helper declraration
;;

let parse_application pack =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let function_parser =
        choice
          [ parens @@ pack.parse_fun pack
          ; parens @@ pack.parse_if_then_else pack
          ; parens @@ pack.parse_match_with pack
          ; parse_ident
          ]
      and operand_parser =
        choice
          [ parens @@ pack.parse_bin_op pack
          ; parens @@ pack.parse_un_op pack
          ; pack.parse_tuple pack
          ; pack.parse_list pack
          ; parens @@ pack.parse_list_cons pack
          ; parens self
          ; parens @@ pack.parse_fun pack
          ; parens @@ parse_match_with pack
          ; parens @@ pack.parse_if_then_else pack
          ; parse_const
          ; parse_ident
          ]
      in
      let chainl acc = lift (eapplication acc) operand_parser in
      let rec go acc = chainl acc >>= go <|> return acc in
      function_parser >>= fun init -> chainl init >>= fun init -> go init)
;;

let default =
  { parse_bin_op
  ; parse_un_op
  ; parse_list
  ; parse_list_cons
  ; parse_tuple
  ; parse_application
  ; parse_fun
  ; parse_match_with
  ; parse_if_then_else
  ; parse_effect_with_arguments
  ; parse_perform
  ; parse_continue
  }
;;

let parsers input =
  choice
    [ parse_declaration input
    ; parse_bin_op input
    ; parse_un_op input
    ; parse_list input
    ; parse_list_cons input
    ; parse_tuple input
    ; parse_application input
    ; parse_fun input
    ; parse_match_with input
    ; parse_if_then_else input
    ; parse_ident
    ; parse_const
    ; parse_effect_without_arguments
    ; parse_effect_with_arguments input
    ; parse_perform input
    ; parse_continue input
    ]
;;

let parse input = parse_string ~consume:All (many (parsers default) <* skip_wspace) input

type ast_type =
  | DeclarationList
  | MixedList
  | FreeExpression

let determine_ast_type ast =
  match ast with
  | [ expr ] ->
    (match expr with
     | EDeclaration (_, _, None) | ERecDeclaration (_, _, None) -> DeclarationList
     | _ -> FreeExpression)
  | _ ->
    let rec helper ast =
      match ast with
      | [] -> DeclarationList
      | hd :: tl ->
        (match hd with
         | EDeclaration (_, _, None) | ERecDeclaration (_, _, None) -> helper tl
         | _ -> MixedList)
    in
    helper ast
;;
