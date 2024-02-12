(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Values
open Errorsi

module type MONAD_ERROR = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t (* возможно вынести в модуль Syntax *)
end

module Env (M: MONAD_ERROR) = struct
  open M

  type t = enviroment

  (* let find_var env name =
    match  with
    | pattern -> pattern *)

  let empty : t = Base.Map.empty (module Base.String)

  let find env k = Base.Map.find env k
  let extend env key value = Base.Map.update env key ~f:(fun _ -> value)
  
  let find_var env name =
    match find env name with
    | Some v -> return (env, v)
    | None -> fail ("ddd") (* UNBOUND VALUE *)
  ;;
end

module Interpreter (M: MONAD_ERROR) = struct
  open M
  open Env(M)

  let eval_const env = function
  (* нужно in в eval или пусть глобальной будет? *)
  | Int i -> return (env, vint i)
  | Bool b -> return (env, vbool b)
  | Unit -> return (env, vunit)
  | Char c -> return (env, vchar c)
  | String s -> return (env, vstring s)
  | _ -> fail (non_existen_type)
  ;;

  let eval_bin_op env = function
  | Add, VInt i1, VInt i2 -> return (env, vint (i1 + i2))
  | Sub, VInt i1, VInt i2 -> return (env, vint (i1 - i2))
  | Mul, VInt i1, VInt i2 -> return (env, vint (i1 * i2))
  | Div, VInt i1, VInt i2 -> 
    let res = 
      match i2 with
      | 0 -> fail(devision_by_zero)
      | other -> return (env, vint (i1 / i2))
    in res
  | And, VBool b1, VBool b2 -> return (env, vbool (b1 && b2))
  | Or, VBool b1, VBool b2 -> return (env, vbool (b1 || b2))

  | Add, _, _ | Sub, _, _ | Mul, _, _ | Div, _, _ | And, _, _ | Or, _, _ ->
    fail(type_error)
  | _ -> fail(non_existen_operation)
  ;;

  let eval_un_op env = function
  | Plus, VInt i -> return (env, vint (+i))
  | Minus, VInt i -> return (env, vint (-i))
  | Not, VBool b -> return (env, vbool (not b))
  | Plus, _ | Minus, _ | Not, _ -> fail(type_error)
  | _ -> fail (non_existen_operation)

  let eval =
    let rec helper env = function
      | EConst c -> eval_const env c
      | EIdentifier name -> find_var env name
      | EUnaryOperation (op, expr) ->
        let* _, v = helper env expr in
        let res = eval_un_op env (op, v) in
        res
      | EBinaryOperation (op, expr1, expr2) ->
        let* _, v1 = helper env expr1 in
        let* _, v2 = helper env expr2 in
        let res = eval_bin_op env (op, v1, v2) in 
        res
      | EIfThenElse (cond, b1, b2) ->
        let* _, v = helper env cond in
        let res = 
        match v with
        | VBool true -> helper env b1
        | VBool false -> helper env b2
        in res
      | EFun (pat, expr) -> return (env, (vfun pat expr env))
      | EDeclaration (name, expr, None) ->
        let* env, v = helper env expr in
        let new_env = extend env name v in
        return (new_env, v)
    in
    helper
  ;;

  let interpret_expr env expr =
    let* env, v = eval env expr in
    return (env, v)
  ;;

  let int_expr expr = interpret_expr empty expr

end

module Eval : MONAD_ERROR with type ('a, 'err) t = ('a, 'err) Result.t = struct
  type ('a, 'err) t = ('a, 'err) Result.t

  let return a = Result.Ok a
  let fail err = Result.Error err

  let ( >>= ) a f =
    match a with
    | Result.Ok v -> f v
    | Result.Error err -> fail err
  ;;
  let ( let* ) = ( >>= )
end

module InterpreterR = Interpreter (Eval)

let int_expr = InterpreterR.int_expr