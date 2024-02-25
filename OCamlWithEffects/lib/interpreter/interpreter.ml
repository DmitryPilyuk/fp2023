(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
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
    | Some v -> return v
    | None -> fail (unbound_variable name)
  ;;

  let find_effect env name =
    match find env name with
    | Some v -> return v
    | None -> fail (unbound_variable name)
  ;;
  (* другая ошибка *)

  (* UNBOUND VALUE *)
end

module Handlers (M : MONAD_ERROR) = struct
  open M

  type t = handlers

  let empty_handler : t = Base.Map.empty (module Base.String)
  let find_handler env k = Base.Map.find env k
  let extend_handler env key value = Base.Map.update env key ~f:(fun _ -> value)

  let compose_handlers env1 env2 =
    Base.Map.fold env2 ~init:env1 ~f:(fun ~key ~data acc_env ->
      extend_handler acc_env key data)
  ;;

  let find_handler handlers name =
    match find_handler handlers name with
    | Some v -> return v
    | None -> fail type_error
  ;;
  (* другую ошибку *)
end

module Interpreter (M : MONAD_ERROR) = struct
  open M
  open Env (M)
  open Handlers (M)

  let eval_const env handlers = function
    (* нужно in в eval или пусть глобальной будет? *)
    | Int i -> return (env, handlers, vint i)
    | Bool b -> return (env, handlers, vbool b)
    | Unit -> return (env, handlers, vunit)
    | Char c -> return (env, handlers, vchar c)
    | String s -> return (env, handlers, vstring s)
  ;;

  let eval_bin_op env handlers = function
    | Add, VInt i1, VInt i2 -> return (env, handlers, vint (i1 + i2))
    | Sub, VInt i1, VInt i2 -> return (env, handlers, vint (i1 - i2))
    | Mul, VInt i1, VInt i2 -> return (env, handlers, vint (i1 * i2))
    | Div, VInt i1, VInt i2 ->
      let res =
        match i2 with
        | 0 -> fail devision_by_zero
        | _ -> return (env, handlers, vint (i1 / i2))
      in
      res
    | And, VBool b1, VBool b2 -> return (env, handlers, vbool (b1 && b2))
    | Or, VBool b1, VBool b2 -> return (env, handlers, vbool (b1 || b2))
    | Eq, VInt i1, VInt i2 -> return (env, handlers, vbool (i1 = i2))
    | Eq, VChar c1, VChar c2 -> return (env, handlers, vbool (c1 = c2))
    | Eq, VString s1, VString s2 -> return (env, handlers, vbool (s1 = s2))
    | Eq, VBool b1, VBool b2 -> return (env, handlers, vbool (b1 = b2))
    | NEq, VInt i1, VInt i2 -> return (env, handlers, vbool (i1 <> i2))
    | NEq, VChar c1, VChar c2 -> return (env, handlers, vbool (c1 <> c2))
    | NEq, VString s1, VString s2 -> return (env, handlers, vbool (s1 <> s2))
    | NEq, VBool b1, VBool b2 -> return (env, handlers, vbool (b1 <> b2))
    | Gt, VInt i1, VInt i2 -> return (env, handlers, vbool (i1 > i2))
    | Lt, VInt i1, VInt i2 -> return (env, handlers, vbool (i1 < i2))
    | Gte, VInt i1, VInt i2 -> return (env, handlers, vbool (i1 >= i2))
    | Lte, VInt i1, VInt i2 -> return (env, handlers, vbool (i1 <= i2))
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

  let eval_un_op env handlers = function
    | Plus, VInt i -> return (env, handlers, vint (+i))
    | Minus, VInt i -> return (env, handlers, vint (-i))
    | Not, VBool b -> return (env, handlers, vbool (not b))
    | Plus, _ | Minus, _ | Not, _ -> fail type_error
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
      let helper =
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
        | PEffectWithoutArguments name1, VEffectWithoutArguments name2 ->
          (match name1 = name2 with
           | true -> return (Successful, env)
           | false -> return (UnSuccessful, env))
        | PEffectWithArguments (name1, pattern1), VEffectWithArguments (name2, value2) ->
          (match name1 = name2 with
           | false -> return (UnSuccessful, env)
           | true ->
             let* flag, new_env = eval_pattern pattern1 value2 in
             (match flag with
              | Successful -> return (Successful, new_env)
              | UnSuccessful -> return (UnSuccessful, env)))
        | _ -> fail type_error
        (* ВОЗМОЖНО ИЗМЕНИТЬ НА ОШИБКУ МЭТЧА *)
      in
      helper
    ;;

    let rec eval_handler handler v =
      let env = empty in
      let rec helper =
        match handler, v with
        (* | EffectHandler (p1, _, _), VHandlerWithoutContinue (p2) ->
           (* переделать: p2 (pattern) сделать value *)
           (match p1 = p2 with
           | true -> return (Successful, env)
           | false -> return (UnSuccessful, env)) *)
        | EffectHandler (p, _, _), (VEffectWithoutArguments _ as v) -> eval_pattern p v
        | EffectHandler (p, _, _), (VEffectWithArguments _ as v) -> eval_pattern p v
      in
      helper
    ;;
  end

  let eval =
    let rec helper env handlers = function
      | EConst c -> eval_const env handlers c
      | EIdentifier name ->
        let* v = find_var env name in
        return (env, handlers, v)
      | EUnaryOperation (op, expr) ->
        let* _, _, v = helper env handlers expr in
        let res = eval_un_op env handlers (op, v) in
        res
      | EBinaryOperation (op, expr1, expr2) ->
        let* _, _, v1 = helper env handlers expr1 in
        let* _, _, v2 = helper env handlers expr2 in
        let res = eval_bin_op env handlers (op, v1, v2) in
        res
      | EIfThenElse (cond, b1, b2) ->
        let* _, _, v = helper env handlers cond in
        let res =
          match v with
          | VBool true -> helper env handlers b1
          | VBool false -> helper env handlers b2
          | _ -> fail type_error (* Мб другая ошибка *)
        in
        res
      | EFun (pat, expr) -> return (env, handlers, vfun pat expr env)
      | ETuple expr_list ->
        (* насчет инвайромента подумать *)
        let* env, values = list_and_tuple_helper env expr_list in
        return (env, handlers, vtuple values)
      | EList expr_list ->
        (* насчет инвайромента подумать *)
        let* env, values = list_and_tuple_helper env expr_list in
        return (env, handlers, vlist values)
      | EListCons (e1, e2) ->
        let* _, _, v1 = helper env handlers e1 in
        let* _, _, v2 = helper env handlers e2 in
        let* values =
          match v2 with
          | VList v -> return (v1 :: v)
          | _ -> fail type_error
        in
        return (env, handlers, vlist values)
      | EDeclaration (name, expr, None) ->
        let* env, _, v = helper env handlers expr in
        let new_env = extend env name v in
        return (new_env, handlers, v)
      | EDeclaration (name, expr, Some expression) ->
        let* env, _, v = helper env handlers expr in
        let new_env = extend env name v in
        let* _, _, v = helper new_env handlers expression in
        return (env, handlers, v)
      | ERecDeclaration (name, expr, None) ->
        rec_declaration_helper env handlers name expr
      | ERecDeclaration (name, expr, Some expression) ->
        let* new_env, _, _ = rec_declaration_helper env handlers name expr in
        let* _, _, v = helper new_env handlers expression in
        return (env, handlers, v)
      | EApplication (f, e) ->
        (* Если будет время, вынести checker в отдельную взаимно-рекурсивную с helper функцию *)
        let* _, _, v1 = helper env handlers f in
        let* _, _, v2 = helper env handlers e in
        (match v1 with
         | VFun (pat, exp, fun_env) ->
           let* flag, pat_env = Pattern.eval_pattern pat v2 in
           let checker =
             match flag with
             | Successful ->
               let new_env = compose fun_env pat_env in
               let* _, _, v = helper new_env handlers exp in
               return (env, handlers, v)
             | UnSuccessful -> fail type_error (* Другая ошибка *)
           in
           checker
         | VRecFun (name, v) ->
           (match v with
            | VFun (pat, exp, fun_env) ->
              let fun_env = extend fun_env name v1 in
              let* flag, pat_env = Pattern.eval_pattern pat v2 in
              checker flag fun_env pat_env env handlers exp
            | _ -> fail type_error)
         | _ -> fail type_error)
      | EEffectDeclaration (name, _) ->
        let v = veffect_declaration name in
        let new_env = extend env name v in
        return (new_env, handlers, v)
      | EEffectWithArguments (name, expr) ->
        let* _, _, v1 = helper env handlers expr in
        let v2 = veffect_with_arguments name v1 in
        return (env, handlers, v2)
      | EEffectWithoutArguments name ->
        let v = veffect_without_arguments name in
        return (env, handlers, v)
      | ETryWith (expr, body) ->
        let rec trywith_helper handlers body =
          match body with
          | [] -> return handlers
          | hd :: tl ->
            (match hd with
             | EffectHandler (pat, expr, cont) ->
               let* handlers =
                 match pat with
                 | PEffectWithoutArguments name as pat ->
                   return (extend_handler handlers name (pat, expr, cont))
                 | PEffectWithArguments (name, _) as pat ->
                   return (extend_handler handlers name (pat, expr, cont))
               in
               trywith_helper handlers tl
             | _ -> fail type_error)
          (* в try_with могут быть только handler *)
        in
        let* handlers = trywith_helper handlers body in
        let* _, _, v = helper env handlers expr in
        return (env, handlers, v)
      | EEffectContinue (cont, expr) ->
        let* _, _, v = helper env handlers expr in
        let* cont =
          match cont with
          | Continue k -> return k
          | _ -> fail `Type_error
        in
        (* !!! *)
        let res =
          match find env cont with
          | Some (VEffectContinue (Continue n)) when n = cont ->
            return (env, handlers, vthrowing_value v)
          | _ -> fail `Type_error
          (* ДРУГУЮ ОШИБКУ КИДАТЬ *)
        in
        res
      | EEffectPerform expr ->
        let* _, _, v = helper env handlers expr in
        (match v with
         | VEffectWithArguments (name, _) | VEffectWithoutArguments name ->
           let* effect_checker = find_effect env name in
           let* handler = find_handler handlers name in
           (match handler with
            | pat, expr, cont ->
              let* flag, pat_env = Pattern.eval_pattern pat v in
              (match flag with
               | Successful ->
                 let new_env = compose env pat_env in
                 let* cont_val =
                   match cont with
                   | Continue k -> return k
                   | _ -> fail type_error
                 in
                 (* другая ошибка *)
                 let new_env = extend new_env cont_val (veffect_continue cont) in
                 let* _, _, v = helper new_env handlers expr in
                 (match v with
                  | VThrowingValue n -> return (env, handlers, n)
                  | _ -> fail (handler_without_continue name))
               | UnSuccessful -> fail type_error)
              (* другая ошибка *)
            | _ -> fail type_error)
         | _ -> fail type_error)
        (* в перформ может быть только эффект *)
      | EMatchWith (expr, cases) ->
        (* Если будет время, добавить обработку случая,
           когда паттерн мэтчится с эксрешеном,
           но при этом следующий паттерн являтся не допустимым
           и нужно кинуть ошибку.

           Делать в последнюю очередь, так как это уже обрабатывается в inferece.
        *)
        let* _, _, v = helper env handlers expr in
        let rec match_cases env = function
          | [] -> fail type_error (* Исправить потом на другую ошибку *)
          | (pat, expr) :: rest ->
            let* flag, env' = Pattern.eval_pattern pat v in
            (match flag with
             | Pattern.Successful ->
               let env'' = compose env env' in
               let* _, _, result = helper env'' handlers expr in
               return (env, handlers, result)
             | Pattern.UnSuccessful -> match_cases env rest)
        in
        match_cases env cases
    and list_and_tuple_helper env = function
      | [] -> return (env, [])
      | expr :: rest ->
        (* насчет инвайромента подумать *)
        let* env, _, value = helper env empty_handler expr in
        let* env, rest_values = list_and_tuple_helper env rest in
        return (env, value :: rest_values)
    and rec_declaration_helper env handlers name expr =
      let* env, _, v = helper env handlers expr in
      let res =
        match v with
        | VFun (_, _, _) -> vrecfun name v
        | _ -> v
      in
      let new_env = extend env name res in
      return (new_env, handlers, res)
    and checker flag fun_env pat_env env handlers exp =
      match flag with
      | Successful ->
        let new_env = compose fun_env pat_env in
        let* _, _, v = helper new_env handlers exp in
        return (env, handlers, v)
      | UnSuccessful -> fail type_error (* Другая ошибка *)
    in
    helper
  ;;

  let interpret_expr expr =
    let* _, _, v = eval empty empty_handler expr in
    return v
  ;;

  let interpret_program program =
    let rec run_helper env handlers = function
      | [] -> return env
      | expr :: rest ->
        let* new_env, _, _ = eval env handlers expr in
        run_helper new_env handlers rest
    in
    run_helper empty empty_handler program
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
