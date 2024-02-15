(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Values
open Errorsi

module type MONAD_ERROR = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

  val ( let* )
    :  ('a, 'e) t
    -> ('a -> ('b, 'e) t)
    -> ('b, 'e) t (* возможно вынести в модуль Syntax *)
end

module Env (M : MONAD_ERROR) = struct
  open M

  type t = enviroment

  let empty : t = Base.Map.empty (module Base.String)
  let find env k = Base.Map.find env k
  let extend env key value = Base.Map.update env key ~f:(fun _ -> value)

  let compose env1 env2 =
    Base.Map.fold env2 ~init:env1 ~f:(fun ~key ~data acc_env -> extend acc_env key data)
  ;;

  let find_var env name =
    match find env name with
    | Some v -> return (env, v)
    | None -> fail (unbound_variable name)
  ;;

  (* UNBOUND VALUE *)
end

module Interpreter (M : MONAD_ERROR) = struct
  open M
  open Env (M)

  let eval_const env = function
    (* нужно in в eval или пусть глобальной будет? *)
    | Int i -> return (env, vint i)
    | Bool b -> return (env, vbool b)
    | Unit -> return (env, vunit)
    | Char c -> return (env, vchar c)
    | String s -> return (env, vstring s)
    | _ -> fail non_existen_type
  ;;

  let eval_bin_op env = function
    | Add, VInt i1, VInt i2 -> return (env, vint (i1 + i2))
    | Sub, VInt i1, VInt i2 -> return (env, vint (i1 - i2))
    | Mul, VInt i1, VInt i2 -> return (env, vint (i1 * i2))
    | Div, VInt i1, VInt i2 ->
      let res =
        match i2 with
        | 0 -> fail devision_by_zero
        | other -> return (env, vint (i1 / i2))
      in
      res
    | And, VBool b1, VBool b2 -> return (env, vbool (b1 && b2))
    | Or, VBool b1, VBool b2 -> return (env, vbool (b1 || b2))
    | Eq, VInt i1, VInt i2 -> return (env, vbool (i1 = i2))
    | Eq, VChar c1, VChar c2 -> return (env, vbool (c1 = c2))
    | Eq, VString s1, VString s2 -> return (env, vbool (s1 = s2))
    | Eq, VBool b1, VBool b2 -> return (env, vbool (b1 = b2))
    | NEq, VInt i1, VInt i2 -> return (env, vbool (i1 <> i2))
    | NEq, VChar c1, VChar c2 -> return (env, vbool (c1 <> c2))
    | NEq, VString s1, VString s2 -> return (env, vbool (s1 <> s2))
    | NEq, VBool b1, VBool b2 -> return (env, vbool (b1 <> b2))
    | Gt, VInt i1, VInt i2 -> return (env, vbool (i1 > i2))
    | Lt, VInt i1, VInt i2 -> return (env, vbool (i1 < i2))
    | Gte, VInt i1, VInt i2 -> return (env, vbool (i1 >= i2))
    | Lte, VInt i1, VInt i2 -> return (env, vbool (i1 <= i2))
    | Add, _, _
    | Sub, _, _
    | Mul, _, _
    | Div, _, _
    | And, _, _
    | Or, _, _
    | Eq, _, _
    | NEq, _, _
    | Gt, _, _
    | Lt, _, _
    | Gte, _, _
    | Lte, _, _ -> fail type_error
  ;;

  let eval_un_op env = function
    | Plus, VInt i -> return (env, vint (+i))
    | Minus, VInt i -> return (env, vint (-i))
    | Not, VBool b -> return (env, vbool (not b))
    | Plus, _ | Minus, _ | Not, _ -> fail type_error
    | _ -> fail non_existen_operation
  ;;

  module Pattern = struct
    type match_flag =
      | Successful
      | UnSuccessful

    let eval_const_pattern env pat v =
      (* ПЕРЕДАВАТЬ ENV ИЛИ ВСЕГДА ДЛЕТАЬ ПУСТОЙ? *)
      match pat, v with
      | Int i1, VInt i2 when i1 = i2 -> return (Successful, env)
      | Bool b1, VBool b2 when b1 = b2 -> return (Successful, env)
      | Char c1, VChar c2 when c1 = c2 -> return (Successful, env)
      | String s1, VString s2 when s1 = s2 -> return (Successful, env)
      | Unit, VUnit -> return (Successful, env)
      | Int _, VInt _ | Bool _, VBool _ | Char _, VChar _ | String _, VString _ ->
        return (UnSuccessful, env)
      | _ -> fail type_error
    ;;

    (* ВОЗМОЖНО ИЗМЕНИТЬ НА ОШИБКУ МЭТЧА *)

    let rec eval_pattern pat v =
      let env = empty in
      let rec helper =
        match pat, v with
        | PAny, _ -> return (Successful, env)
        | PConst i, v -> eval_const_pattern env i v
        | PNill, VList [] -> return (Successful, env)
        | PVal name, v ->
          let new_env = extend env name v in
          return (Successful, new_env)
        | PTuple pats, VTuple vs ->
          let rec match_tuple env = function
            | [], [] -> return (Successful, env)
            | pat :: pats, v :: vs ->
              let* flag, pat_env = eval_pattern pat v in
              let new_env = compose env pat_env in
              let result =
                match flag with
                | Successful -> match_tuple new_env (pats, vs)
                | UnSuccessful -> return (UnSuccessful, new_env)
              in
              result
            | _ -> return (UnSuccessful, env)
          in
          match_tuple env (pats, vs)
        | PListCons (pat1, pat2), VList (v1 :: v2) ->
          let* flag1, pat_env1 = eval_pattern pat1 v1 in
          let* flag2, pat_env2 = eval_pattern pat2 (VList v2) in
          let* env' =
            match flag1, flag2 with
            | Successful, Successful ->
              let combined_env = compose pat_env1 pat_env2 in
              return combined_env
            | _ -> fail type_error (* Возможно, стоит вернуть другую ошибку*)
          in
          return (Successful, env')
        | _ -> fail type_error
        (* ВОЗМОЖНО ИЗМЕНИТЬ НА ОШИБКУ МЭТЧА *)
      in
      helper
    ;;
  end

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
        in
        res
      | EFun (pat, expr) -> return (env, vfun pat expr env)
      | ETuple expr_list ->
        (* насчет инвайромента подумать *)
        let* env, values = list_and_tuple_helper env expr_list in
        return (env, vtuple values)
      | EList expr_list ->
        (* насчет инвайромента подумать *)
        let* env, values = list_and_tuple_helper env expr_list in
        return (env, vlist values)
      | EListCons (e1, e2) ->
        let* _, v1 = helper env e1 in
        let* _, v2 = helper env e2 in
        let* values =
          match v2 with
          | VList v -> return (v1 :: v)
          | _ -> fail type_error
        in
        return (env, vlist values)
      | EDeclaration (name, expr, None) ->
        let* env, v = helper env expr in
        let new_env = extend env name v in
        return (new_env, v)
      | EDeclaration (name, expr, Some expression) ->
        let* env, v = helper env expr in
        let new_env = extend env name v in
        let* _, v = helper new_env expression in
        return (env, v)
      | ERecDeclaration (name, expr, None) ->
        let* env, v = helper env expr in
        let new_env = extend env name v in
        return (new_env, v)
      | ERecDeclaration (name, expr, Some expression) ->
        let* env, v = helper env expr in
        let new_env = extend env name v in
        let* _, v = helper new_env expression in
        return (env, v)
      | EApplication (f, e) ->
        let* _, v1 = helper env f in
        let* _, v2 = helper env e in
        (match v1 with
         | VFun (pat, exp, fun_env) ->
           let* flag, pat_env = Pattern.eval_pattern pat v2 in
           let new_env = compose fun_env pat_env in
           (* let* nenv, v = helper new_env exp in
              return (env, v) *)
           let checker =
             match flag with
             | Successful ->
               let new_env = compose fun_env pat_env in
               let* _, v = helper new_env exp in
               return (env, v)
             | UnSuccessful -> fail type_error
             (* ДРУГУЮ ОШИБКУ *)
           in
           checker (* ИСПРАВИТЬ *)
         | _ -> fail type_error)
      | EMatchWith (expr, cases) ->
        (* Добавить обработку случая,
           когда паттерн мэтчится с эксрешеном,
           но при этом следующий паттерн являтся не допустимым
           и нужно кинуть ошибку *)
        let* _, v = helper env expr in
        let rec match_cases env = function
          | [] -> fail type_error (* Исправить потом на другую ошибку *)
          | (pat, expr) :: rest ->
            let* flag, env' = Pattern.eval_pattern pat v in
            (match flag with
             | Pattern.Successful ->
               let* _, result = helper env' expr in
               return (env, result)
             | Pattern.UnSuccessful -> match_cases env rest)
        in
        match_cases env cases
    and list_and_tuple_helper env = function
      | [] -> return (env, [])
      | expr :: rest ->
        (* насчет инвайромента подумать *)
        let* env, value = helper env expr in
        let* env, rest_values = list_and_tuple_helper env rest in
        return (env, value :: rest_values)
    in
    helper
  ;;

  let interpret_expr expr =
    let* env, v = eval empty expr in
    return v
  ;;

  let interpret_program program =
    let rec run_helper env = function
      | [] -> return env
      | expr :: rest ->
        let* new_env, _ = eval env expr in
        run_helper new_env rest
    in
    run_helper empty program
  ;;

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

let run_expr_interpreter = InterpreterR.interpret_expr
let run_program_interpreter = InterpreterR.interpret_program
