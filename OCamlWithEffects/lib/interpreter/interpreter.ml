(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Values
open Int_errors

module type MONAD_ERROR = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
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
    | None -> fail (unbound_effect name)
  ;;
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
    | None -> fail (unbound_handler name)
  ;;
end

module Pattern (M : MONAD_ERROR) = struct
  open M
  open Env (M)

  type match_flag =
    | Successful
    | UnSuccessful

  let eval_const_pattern env pat v =
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
      | PNill, _ -> return (UnSuccessful, env)
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
end

module Interpreter (M : MONAD_ERROR) = struct
  open M
  open Env (M)
  open Handlers (M)
  open Pattern (M)

  let eval_const env = function
    | Int i -> return (env, vint i)
    | Bool b -> return (env, vbool b)
    | Unit -> return (env, vunit)
    | Char c -> return (env, vchar c)
    | String s -> return (env, vstring s)
  ;;

  let eval_un_op env = function
    | Plus, VInt i -> return (env, vint (+i))
    | Minus, VInt i -> return (env, vint (-i))
    | Not, VBool b -> return (env, vbool (not b))
    | Plus, _ | Minus, _ | Not, _ -> fail type_error
  ;;

  let eval_bin_op env = function
    (* + - * / *)
    | Add, VInt i1, VInt i2 -> return (env, vint (i1 + i2))
    | Sub, VInt i1, VInt i2 -> return (env, vint (i1 - i2))
    | Mul, VInt i1, VInt i2 -> return (env, vint (i1 * i2))
    | Div, VInt i1, VInt i2 ->
      let res =
        match i2 with
        | 0 -> fail division_by_zero
        | _ -> return (env, vint (i1 / i2))
      in
      res
    (* && || *)
    | And, VBool b1, VBool b2 -> return (env, vbool (b1 && b2))
    | Or, VBool b1, VBool b2 -> return (env, vbool (b1 || b2))
    (* = *)
    | Eq, VInt i1, VInt i2 -> return (env, vbool (i1 = i2))
    | Eq, VChar c1, VChar c2 -> return (env, vbool (c1 = c2))
    | Eq, VString s1, VString s2 -> return (env, vbool (s1 = s2))
    | Eq, VBool b1, VBool b2 -> return (env, vbool (b1 = b2))
    | Eq, VUnit, VUnit -> return (env, vbool true)
    (* <> or != *)
    | NEq, VInt i1, VInt i2 -> return (env, vbool (i1 <> i2))
    | NEq, VChar c1, VChar c2 -> return (env, vbool (c1 <> c2))
    | NEq, VString s1, VString s2 -> return (env, vbool (s1 <> s2))
    | NEq, VBool b1, VBool b2 -> return (env, vbool (b1 <> b2))
    | NEq, VUnit, VUnit -> return (env, vbool false)
    (* > *)
    | Gt, VInt i1, VInt i2 -> return (env, vbool (i1 > i2))
    | Gt, VBool _, VBool _ -> return (env, vbool false)
    | Gt, VChar c1, VChar c2 -> return (env, vbool (c1 > c2))
    | Gt, VString s1, VString s2 -> return (env, vbool (s1 > s2))
    | Gt, VUnit, VUnit -> return (env, vbool false)
    (* < *)
    | Lt, VInt i1, VInt i2 -> return (env, vbool (i1 < i2))
    | Lt, VBool _, VBool _ -> return (env, vbool false)
    | Lt, VChar c1, VChar c2 -> return (env, vbool (c1 < c2))
    | Lt, VString s1, VString s2 -> return (env, vbool (s1 < s2))
    | Lt, VUnit, VUnit -> return (env, vbool false)
    (* >= *)
    | Gte, VInt i1, VInt i2 -> return (env, vbool (i1 >= i2))
    | Gte, VBool _, VBool _ -> return (env, vbool false)
    | Gte, VChar c1, VChar c2 -> return (env, vbool (c1 >= c2))
    | Gte, VString s1, VString s2 -> return (env, vbool (s1 >= s2))
    | Gte, VUnit, VUnit -> return (env, vbool false)
    (* <= *)
    | Lte, VInt i1, VInt i2 -> return (env, vbool (i1 <= i2))
    | Lte, VBool _, VBool _ -> return (env, vbool false)
    | Lte, VChar c1, VChar c2 -> return (env, vbool (c1 <= c2))
    | Lte, VString s1, VString s2 -> return (env, vbool (s1 <= s2))
    | Lte, VUnit, VUnit -> return (env, vbool false)
    (* Operand type mismatch. *)
    | _ -> fail type_error
  ;;

  let rec eval_handler handler v =
    let env = empty in
    let rec helper =
      match handler, v with
      | EffectHandler (p, _, _), (VEffectWithoutArguments _ as v) -> eval_pattern p v
      | EffectHandler (p, _, _), (VEffectWithArguments _ as v) -> eval_pattern p v
    in
    helper
  ;;

  let eval =
    let rec helper env handlers = function
      (* Environment in the return value is needed for type compatibility
         and correct operation of EDeclaration and ERecDeclaration. Passing
         handlers to each helper is necessary for type compatibility and
         correct operation of ETryWith. *)
      | EConst c -> eval_const env c
      | EIdentifier name ->
        let* v = find_var env name in
        return (env, v)
      | EUnaryOperation (op, expr) ->
        let* _, v = helper env handlers expr in
        let res = eval_un_op env (op, v) in
        res
      | EBinaryOperation (op, expr1, expr2) ->
        let* _, v1 = helper env handlers expr1 in
        let* _, v2 = helper env handlers expr2 in
        let res = eval_bin_op env (op, v1, v2) in
        res
      | EIfThenElse (cond, b1, b2) ->
        let* _, v = helper env handlers cond in
        let res =
          match v with
          | VBool true -> helper env handlers b1
          | VBool false -> helper env handlers b2
          | _ -> fail type_error
        in
        res
      | EFun (pat, expr) -> return (env, vfun pat expr env)
      | ETuple expr_list ->
        let* _, values = list_and_tuple_helper env expr_list in
        return (env, vtuple values)
      | EList expr_list ->
        let* _, values = list_and_tuple_helper env expr_list in
        return (env, vlist values)
      | EListCons (e1, e2) ->
        let* _, v1 = helper env handlers e1 in
        let* _, v2 = helper env handlers e2 in
        let* values =
          match v2 with
          | VList v -> return (v1 :: v)
          | _ -> fail type_error
        in
        return (env, vlist values)
      | EDeclaration (name, expr, None) ->
        let* env, v = helper env handlers expr in
        let new_env = extend env name v in
        return (new_env, v)
      | EDeclaration (name, expr, Some expression) ->
        let* env, v = helper env handlers expr in
        let new_env = extend env name v in
        let* _, v = helper new_env handlers expression in
        return (env, v)
      | ERecDeclaration (name, expr, None) ->
        rec_declaration_helper env handlers name expr
      | ERecDeclaration (name, expr, Some expression) ->
        let* new_env, _ = rec_declaration_helper env handlers name expr in
        let* _, v = helper new_env handlers expression in
        return (env, v)
      | EApplication (f, e) ->
        let* _, v1 = helper env handlers f in
        let* _, v2 = helper env handlers e in
        (match v1 with
         | VFun (pat, exp, fun_env) ->
           let* flag, pat_env = eval_pattern pat v2 in
           checker flag fun_env pat_env env handlers exp
         | VRecFun (name, v) ->
           (match v with
            | VFun (pat, exp, fun_env) ->
              let fun_env = extend fun_env name v1 in
              let* flag, pat_env = eval_pattern pat v2 in
              checker flag fun_env pat_env env handlers exp
            | _ -> fail type_error)
         | _ -> fail type_error)
      | EEffectDeclaration (name, _) ->
        let v = veffect_declaration name in
        let new_env = extend env name v in
        return (new_env, v)
      | EEffectWithArguments (name, expr) ->
        let* _ = find_effect env name in
        let* _, v1 = helper env handlers expr in
        let v2 = veffect_with_arguments name v1 in
        return (env, v2)
      | EEffectWithoutArguments name ->
        let* _ = find_effect env name in
        let v = veffect_without_arguments name in
        return (env, v)
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
        let* _, v = helper env handlers expr in
        return (env, v)
      | EEffectContinue (cont, expr) ->
        let* _, v = helper env handlers expr in
        let* cont =
          match cont with
          | Continue k -> return k
          | _ -> fail `Type_error
        in
        (* !!! *)
        let res =
          match find env cont with
          | Some (VEffectContinue (Continue n)) when n = cont ->
            return (env, vthrowing_value v)
          | _ -> fail `Type_error
          (* ДРУГУЮ ОШИБКУ КИДАТЬ *)
        in
        res
      | EEffectPerform expr ->
        let* _, v = helper env handlers expr in
        (match v with
         | VEffectWithArguments (name, _) | VEffectWithoutArguments name ->
           let* _ = find_effect env name in
           let* handler = find_handler handlers name in
           (match handler with
            | pat, expr, cont ->
              let* flag, pat_env = eval_pattern pat v in
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
                 let* _, v = helper new_env handlers expr in
                 (match v with
                  | VThrowingValue n -> return (env, n)
                  | _ -> fail (handler_without_continue name))
               | UnSuccessful -> fail type_error)
              (* другая ошибка *))
         | _ -> fail type_error)
        (* в перформ может быть только эффект *)
      | EMatchWith (expr, cases) ->
        let* _, v = helper env handlers expr in
        let rec match_cases env = function
          | [] -> fail type_error (* Исправить потом на другую ошибку *)
          | (pat, expr) :: rest ->
            let* flag, env' = eval_pattern pat v in
            (match flag with
             | Successful ->
               let env'' = compose env env' in
               let* _, result = helper env'' handlers expr in
               return (env, result)
             | UnSuccessful -> match_cases env rest)
        in
        match_cases env cases
    and list_and_tuple_helper env = function
      | [] -> return (env, [])
      | expr :: rest ->
        (* насчет инвайромента подумать *)
        let* env, value = helper env empty_handler expr in
        let* env, rest_values = list_and_tuple_helper env rest in
        return (env, value :: rest_values)
    and rec_declaration_helper env handlers name expr =
      let* env, v = helper env handlers expr in
      let res =
        match v with
        | VFun (_, _, _) -> vrecfun name v
        | _ -> v
      in
      let new_env = extend env name res in
      return (new_env, res)
    and checker flag fun_env pat_env env handlers exp =
      match flag with
      | Successful ->
        let new_env = compose fun_env pat_env in
        let* _, v = helper new_env handlers exp in
        return (env, v)
      | UnSuccessful -> fail type_error (* Другая ошибка *)
    in
    helper
  ;;

  let interpret_expr expr =
    let* _, v = eval empty empty_handler expr in
    return v
  ;;

  let interpret_program program =
    let rec run_helper env handlers = function
      | [] -> return env
      | expr :: rest ->
        let* new_env, _ = eval env handlers expr in
        run_helper new_env handlers rest
    in
    run_helper empty empty_handler program
  ;;
end

module RESULT_MONAD_ERROR = struct
  include Base.Result

  let ( let* ) = ( >>= )
end

module InterpreterWithResultMonad = Interpreter (RESULT_MONAD_ERROR)

let run_expr_interpreter = InterpreterWithResultMonad.interpret_expr
let run_program_interpreter = InterpreterWithResultMonad.interpret_program
