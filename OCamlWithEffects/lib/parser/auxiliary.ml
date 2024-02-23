(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

(* Determinant of the type of a steamed expression *)

type ast_type =
  | DeclarationList
  | MixedList
  | FreeExpression

let determine_ast_type ast =
  match ast with
  | [ expr ] ->
    (match expr with
     | EDeclaration (_, _, None) | ERecDeclaration (_, _, None) | EEffectDeclaration _ -> DeclarationList
     | _ -> FreeExpression)
  | _ ->
    let rec helper ast =
      match ast with
      | [] -> DeclarationList
      | hd :: tl ->
        (match hd with
         | EDeclaration (_, _, None) | ERecDeclaration (_, _, None) | EEffectDeclaration _ -> helper tl
         | _ -> MixedList)
    in
    helper ast
;;

(* ---------------- *)

(* Helper functions for parsing *)

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

(* ---------------- *)

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
let edeclaration func_name expression exp_in = EDeclaration (func_name, expression, exp_in)
let erec_declaration func_name expression exp_in = ERecDeclaration (func_name, expression, exp_in)
let eif_then_else condition true_b false_b = EIfThenElse (condition, true_b, false_b)
let ematch_with expression cases = EMatchWith (expression, cases)
let etry_with expression handlers = ETryWith (expression, handlers)
let eeffect_without_arguments name = EEffectWithoutArguments name
let eefect_with_arguments name arg = EEffectWithArguments (name, arg)
let eeffect_declaration name typ = EEffectDeclaration (name, typ)
let eeffect_perform expr = EEffectPerform expr
let eeffect_continue cont expr = EEffectContinue (cont, expr)
let econt_val n = Continue n

(* ---------------- *)

(* Constructors for binary operations *)

let sadd _ = Add
let ssub _ = Sub
let smul _ = Mul
let sdiv _ = Div
let seq _ = Eq
let sneq _ = NEq
let sgt _ = Gt
let sgte _ = Gte
let slt _ = Lt
let slte _ = Lte
let sand _ = And
let sor _ = Or

(* ---------------- *)

(* Constructors for unary operations *)

let umin _ = Minus
let unot _ = Not
let uplus _ = Plus

(* ---------------- *)

(* Constructors for patterns *)

let pany _ = PAny
let pnill _ = PNill
let pconst c = PConst c
let pval v = PVal v
let plist_cons l r = PListCons (l, r)
let ptuple l = PTuple l
let peffect_without_args name = PEffectWithoutArguments name
let peffect_with_args name a = PEffectWithArguments (name, a)

(* ---------------- *)

(* Constructors for annotations *)

let aint = AInt
let abool = ABool
let achar = AChar
let astring = AString
let aunit = AUnit
let aarrow l r = AArrow (l, r)
let alist a = AList a
let atuple alist = ATuple alist
let aeffect a = AEffect a

(* ----------------- *)

(* Constructors for effect handlers *)

let effecthandler pat expr cont = EffectHandler (pat, expr, cont)
let continue k = Continue k

(* ----------------- *)