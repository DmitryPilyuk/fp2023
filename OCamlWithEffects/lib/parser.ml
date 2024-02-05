(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

type dispatch =
  { parse_un_op : dispatch -> expr Angstrom.t
  ; parse_bin_op : dispatch -> expr Angstrom.t
  ; parse_list : dispatch -> expr Angstrom.t
  ; parse_tuple : dispatch -> expr Angstrom.t
  ; parse_application : dispatch -> expr Angstrom.t
  ; parse_fun : dispatch -> expr Angstrom.t
  ; parse_if_then_else : dispatch -> expr Angstrom.t
  }

(* Constructors for expressions *)
let econst x = EConst x
let ebinop op left_op right_op = EBinaryOperation (op, left_op, right_op)
let eunop operator operand = EUnaryOperation (operator, operand)
let elist cont = EList cont
let etuple cont = ETuple cont
let eidentifier x = EIdentifier x
let eapplication f x = EApplication (f, x)
let efun var_list expression = EFun (var_list, expression)

let declraration func_name var_list expression =
  EDeclaration (func_name, var_list, expression)
;;

let rec_declraration func_name var_list expression =
  ERecDeclaration (func_name, var_list, expression)
;;

let eif_then_else condition true_b false_b = EIfThenElse (condition, true_b, false_b)
let ematch_with expression cases = EMatchWith (expression, cases)
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
let uNot _  = Not
let uPlus _ = Plus
(* ---------------- *)

(* Constructors for patterns *)
let pAny _ = PAny
let pNill _ = PNill
let pConst c = PConst c
let pVal v = PVal v
(* ---------------- *)

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
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

let is_letter c = is_upper c || is_lower c
let parens p = skip_wspace *> char '(' *> p <* skip_wspace <* char ')'
let sqr_parens p = skip_wspace *> char '[' *> p <* skip_wspace <* char ']'
let braces p = skip_wspace *> char '{' *> p <* skip_wspace <* char '}'

let list_sep =
  skip_wspace *> char ';' *> skip_wspace
  <|> (skip_wspace *> peek_char
       >>= function
       | Some c when c != ']' -> fail "Error: Expected semicolon or end of list"
       | _ -> return ())
;;

let tuple_sep =
  skip_wspace *> char ',' *> skip_wspace
  <|> (skip_wspace *> peek_char
       >>= function
       | Some c when c != ')' -> fail "Error: Expected comma or end of tuple"
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

let parse_ident =
  skip_wspace *> peek_char
  >>= is_acceptable_fl
  >>= fun _ ->
  take_while is_ident
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: name is used as keyword"
  else return @@ eidentifier s
;;

let parse_const =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let parse_int = take_while1 is_digit >>| int_of_string >>| fun x -> Int x
      and parse_str =
        char '"' *> take_while (( != ) '"') <* char '"' >>| fun x -> String x
      and parse_char = char '\'' *> any_char <* char '\'' >>| fun x -> Char x
      and parse_bool =
        string "true" <|> string "false" >>| bool_of_string >>| fun x -> Bool x
      and parse_unit = string "()" >>| fun _ -> Unit in
      let parse_const =
        choice [ parse_int; parse_str; parse_char; parse_bool; parse_unit ]
      in
      lift econst parse_const)
;;

let parse_pattern_nill = (sqr_parens skip_wspace) >>| pNill
let parse_pattern_val = 
  parse_ident >>|
  (function
    | EIdentifier i -> i
    | _ -> "")
  >>| pVal
;;

let parse_fun pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; self
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  parens self
  <|> string "fun"
      *> lift2
           efun
           (many1 parse_uncapitalized_name <* skip_wspace <* string "->" <* skip_wspace)
           (parse_expr <* skip_wspace)
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
      ; pack.parse_tuple pack
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
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
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
  let ( <||> ) = chainl1 in
  parse_expr
  <||> multiplication
  <||> division
  <||> addition
  <||> subtraction
  <||> larger
  <||> largerEq
  <||> less
  <||> lessEq
  <||> eqality
  <||> neqality
  <||> logand
  <||> logor
  >>= fun s ->
  match s with
  | EBinaryOperation (_, _, _) -> return s
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
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in 
  let content = skip_wspace *> many (parse_expr <* list_sep) in
  parens self <|> lift elist @@ sqr_parens @@ content
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
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let content = skip_wspace *> many (parse_expr <* tuple_sep) in
  parens self <|>
  parens parse_const <|>
  lift etuple @@ parens @@ content
;;

let parse_declaration pack =
  fix
  @@ fun _ ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  >>= function
  | "rec" ->
    lift3
      rec_declraration
      parse_uncapitalized_name
      (many parse_uncapitalized_name)
      (skip_wspace *> string "=" *> parse_expr)
  | _ ->
    lift3
      declraration
      parse_uncapitalized_name
      (many parse_uncapitalized_name)
      (skip_wspace *> string "=" *> parse_expr)
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
          ; parse_ident
          ]
      and operand_parser =
        choice
          [ parens @@ pack.parse_bin_op pack
          ; parens @@ pack.parse_un_op pack
          ; pack.parse_tuple pack
          ; pack.parse_list pack
          ; parens self
          ; parens @@ pack.parse_fun pack
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
  ; parse_tuple
  ; parse_application
  ; parse_fun
  ; parse_if_then_else
  }
;;

let parse input =
  parse_string ~consume:All (many (parse_declaration default) <* skip_wspace) input
;;
