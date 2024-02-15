(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Ast
open Errors

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> f v s
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> s, Base.Result.return @@ f v
  ;;

  let return v last = last, Base.Result.return v
  let fail e state = state, Base.Result.fail e
  let bind x ~f = x >>= f
  let fresh : int t = fun last -> last + 1, Result.Ok last

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left mp ~init ~f =
      let open Syntax in
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    let fold_left lt ~init ~f =
      let open Syntax in
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;
  end

  let run m = snd (m 0)
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (left, right) -> occurs_in v left || occurs_in v right
    | TList typ -> occurs_in v typ
    | TTuple typ_list ->
      List.fold_left (fun acc item -> acc || occurs_in v item) false typ_list
    | TEffect typ -> occurs_in v typ
    | TPrim _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar n -> TVarSet.add n acc
      | TArr (left, right) -> helper (helper acc left) right
      | TList typ -> helper acc typ
      | TTuple typ_list -> List.fold_left (fun acc item -> helper acc item) acc typ_list
      | TEffect typ -> helper acc typ
      | TPrim _ -> acc
    in
    helper TVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val find : t -> int -> typ option
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  type t = (int, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find sub k = Base.Map.find sub k
  let remove sub k = Base.Map.remove sub k

  let apply sub =
    let rec helper = function
      | TVar n ->
        (match find sub n with
         | None -> tvar n
         | Some v -> v)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TList typ -> tlist (helper typ)
      | TTuple t_list -> ttuple (Base.List.map t_list ~f:helper)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TArr (left1, right1), TArr (left2, right2) ->
      let* sub1 = unify left1 left2 in
      let* sub2 = unify (apply sub1 right1) (apply sub1 right2) in
      compose sub1 sub2
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TTuple t_list1, TTuple t_list2 ->
      (match
         Base.List.fold2 t_list1 t_list2 ~init:(return empty) ~f:(fun acc it1 it2 ->
           let* sub1 = acc in
           let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
           compose sub1 sub2)
       with
       | Ok r -> r
       | _ -> fail (`Unification_failed (l, r)))
    | TEffect typ1, TEffect typ2 -> unify typ1 typ2
    | _ -> fail (`Unification_failed (l, r))

  and extend k v sub =
    match find sub k with
    | None ->
      let v = apply sub v in
      let* sub2 = singleton k v in
      let f1 ~key ~data acc =
        let* acc = acc in
        let new_data = apply sub2 data in
        return (Base.Map.update acc key ~f:(fun _ -> new_data))
      in
      Base.Map.fold sub ~init:(return sub2) ~f:f1
    | Some vl ->
      let* sub2 = unify v vl in
      compose sub sub2

  and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct

  let free_vars = function
    | Scheme(bind_vars, ty) -> TVarSet.diff (Type.type_vars ty) bind_vars
  ;;

  let occurs_in tvar = function
    | Scheme(bind_vars, ty) -> (not (TVarSet.mem tvar bind_vars)) && Type.occurs_in tvar ty
  ;;

  let apply sub = function
    | Scheme(bind_vars, ty) ->
      let sub2 = TVarSet.fold (fun sub key -> Subst.remove key sub) bind_vars sub in
      Scheme(bind_vars, Subst.apply sub2 ty)
  ;;

end

module TypeEnv = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  let free_vars : t -> TVarSet.t =
    fun env ->
    Base.Map.fold
    ~init:TVarSet.empty
    ~f:(fun ~key:_ ~data acc -> TVarSet.union acc (Scheme.free_vars data))
    env
  ;;

  let extend : t -> string -> scheme -> t =
    fun env key schema ->
      Base.Map.update env key (fun _ -> schema)
  ;;

  let apply : t -> Subst.t -> t =
    fun env sub -> Base.Map.map env ~f:(Scheme.apply sub)
  ;;

  let find = 
    fun env key -> Base.Map.find env key
  ;;

end

open R
open R.Syntax

let fresh_var = fresh >>| fun name -> tvar name

let instantiate : scheme -> typ R.t =
  fun (Scheme (bind_var, ty)) ->
    TVarSet.fold
      (fun var_name acc ->
        let* acc = acc in
        let* fv = fresh_var in
        let* sub = Subst.singleton var_name fv in
        return (Subst.apply sub acc))
      bind_var
      (return ty)
;;

let generalize : TypeEnv.t -> Type.t -> scheme =
  fun env ty ->
    let free = TVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
    Scheme(free, ty)
;;

let lookup_env env var_name =
  match TypeEnv.find env var_name with
  | Some scheme -> 
    let* ty = instantiate scheme in
    return (Subst.empty, ty)
  | None -> fail (`Unbound_variable var_name)
;;

let infer_const c = 
  let ty = match c with
  | Ast.Int _ -> tint
  | Ast.Bool _ -> tbool
  | Ast.Char _ -> tchar
  | Ast.String _ -> tstring
  | Ast.Unit -> tunit in
  return (Subst.empty, ty)
;;

let infer_id env = fun id ->
  match id with
  | "_" -> 
    let* fv = fresh_var in
    return (Subst.empty, fv)
  | _ -> lookup_env env id
;;

let binary_operator_type = fun operator ->
  match operator with
  | Eq | NEq | Gt | Gte | Lt | Lte -> 
    let* fv = fresh_var in return(fv, tbool)
  | Add | Sub | Mul | Div -> return(tint, tint)
  | And | Or -> return(tbool, tbool)
;;

let unary_operator_type = fun operator ->
  match operator with
  | Minus -> tint
  | Plus -> tint
  | Not -> tbool
;;

let infer_pattern =
  let rec helper env = function
  | PVal v -> 
    let* fv = fresh_var in
    let schema = Scheme(TVarSet.empty, fv) in
    let env = TypeEnv.extend env v schema in
    return (fv, env)
  | PAny -> 
    let* fv = fresh_var in
    return (fv, env)
  | PNill ->
    let* fv = fresh_var in
    let ty = tlist fv in
    return (ty, env)
  | PConst c -> 
    let* _, ty = infer_const c in
    return (ty, env)
  | PTuple pattern_list ->
    (* несколько переменных в кортеже *)
    let* ty, env = 
      RList.fold_left
      pattern_list
      ~init:(return([], env))
      ~f:(fun (acc, env) pattern ->
        let* ty1, env1 = helper env pattern in
        return (ty1 :: acc, env1))
    in
    return (ttuple @@ List.rev ty, env)
  | PListCons (l, r) ->
    (* несколько переменных в списке *)
    let* ty1, env1 = helper env l in
    let* ty2, env2 = helper env1 r in
    let* fv = fresh_var in
    let* sub1 = Subst.unify (tlist ty1) fv in
    let* sub2 = Subst.unify ty2 fv in
    let* sub3 = Subst.compose sub1 sub2 in
    let env = TypeEnv.apply env2 sub3 in
    let ty3 = Subst.apply sub3 fv in
    return (ty3, env) 
  in
  helper
;;

let infer_expr =
  let rec helper env = function
  | EConst c -> infer_const c
  | EIdentifier id -> infer_id env id
  | EUnaryOperation (op, expr) ->
    let op = unary_operator_type op in
    let* sub1, ty = helper env expr in
    let* sub2 = Subst.unify ty op in
    let* sub3 = Subst.compose sub1 sub2 in
    return (sub3, op)
  | EBinaryOperation (op, expr1, expr2) ->
    let* args_type, expr_type = binary_operator_type op in
    let* sub_left, ty1 = helper env expr1 in
    let* sub_right, ty2 = helper env expr2 in
    let* sub1 = Subst.unify ty1 args_type in
    let* sub2 = Subst.unify (Subst.apply sub1 ty2) args_type in
    let* sub = Subst.compose_all [ sub_left; sub_right; sub1; sub2] in
    return (sub, expr_type)
  | EFun (pattern, expr) ->
    let* ty1, env1 = infer_pattern env pattern in
    let* sub1, ty2 = helper env1 expr in
    let result = (Subst.apply sub1 ty1) @-> ty2 in
    return (sub1, result)
  | EApplication (func_expr, arg_expr) ->
    let* sub1, func_type = helper env func_expr in
    let* sub2, arg_type = helper (TypeEnv.apply env sub1) arg_expr in
    let* result_type = fresh_var in
    let* unify1 = Subst.unify (Subst.apply sub2 func_type) (arg_type @-> result_type) in
    let* sub3 = Subst.compose_all [sub1; sub2; unify1] in
    return (sub3, Subst.apply sub3 result_type)
  | EIfThenElse (cond, branch1, branch2) ->
    let* sub1, ty1 = helper env cond in
    let* sub2, ty2 = helper env branch1 in
    let* sub3, ty3 = helper env branch2 in
    let* sub4 = Subst.unify ty1 tbool in
    let* sub5 = Subst.unify ty2 ty3 in
    let* sub = Subst.compose_all [sub1; sub2; sub3; sub4; sub5] in
    return (sub, Subst.apply sub ty3)
  | EListCons (l, r) ->
    let* sub1, ty1 = helper env l in
    let env2 = TypeEnv.apply env sub1 in
    let* sub2, ty2 = helper env r in
    let* fv = fresh_var in
    let* sub3 = Subst.unify (tlist ty1) fv in
    let* sub4 = Subst.unify ty2 fv in
    let* sub5 = Subst.compose_all [sub1 ; sub2 ; sub3 ; sub4] in
    let ty = Subst.apply sub5 fv in
    return (sub5, ty)
  | EList expr_list ->
    let* fv = fresh_var in
    let rec infer_list acc = function
      | [] -> return (acc, tlist fv)
      | hd :: tl ->
        let* sub1, ty1 = helper env hd in
        let* sub2 = Subst.unify ty1 fv in
        let* sub = Subst.compose sub1 sub2 in
        infer_list (sub :: acc) tl
    in
    let* subs, ty = infer_list [] expr_list in
    let* final_sub = Subst.compose_all subs in
    return (final_sub, Subst.apply final_sub ty)
  | ETuple expr_list ->
    let rec infer_tuple acc = function
    | [] -> return acc
    | hd :: tl ->
      let* sub1, ty1 = helper env hd in
      let acc_sub, acc_ty = acc in
      let* sub2 = Subst.compose sub1 acc_sub in
      let new_acc = (sub2, ty1 :: acc_ty) in
      infer_tuple new_acc tl
    in
    let acc = Subst.empty, [] in
    let* sub, ty = infer_tuple acc expr_list in
    return (sub, ttuple (List.rev_map (Subst.apply sub) ty))
  | EMatchWith (expr, cases) ->
    let* sub1, ty1 = helper env expr in
    let env2 = TypeEnv.apply env sub1 in
    let* fv = fresh_var in
    let f = fun acc case ->
      let acc_sub, acc_ty = acc in
      let pat, cexpr = case in
      let* pat_ty, pat_env = infer_pattern env2 pat in
      let* sub2 = Subst.unify ty1 pat_ty in
      let env3 = TypeEnv.apply pat_env sub2 in
      let* cexpr_sub, cexpr_ty = helper env3 cexpr in
      let* sub3 = Subst.unify cexpr_ty acc_ty in
      let* sub4 = Subst.compose_all [acc_sub; sub2; sub3; cexpr_sub] in
      let ty = Subst.apply sub4 acc_ty in
      return (sub4, ty) in
    RList.fold_left
    cases
    ~init:(return(sub1, fv))
    ~f:f
  | ERecDeclaration (name, expr1, expr2) as rec_declaration ->
    let* fv = fresh_var in
    let env2 = TypeEnv.extend env name (Scheme(TVarSet.empty, fv)) in
    let* sub1, ty1 = helper env2 expr1 in
    let* sub2 = Subst.unify ty1 fv in
    let* sub3 = Subst.compose sub1 sub2 in
    let ty3 = Subst.apply sub3 fv in
    let result = match rec_declaration with
      | ERecDeclaration (name, expr1, None) -> return (sub3, ty3)
      | ERecDeclaration (name, expr1, Some expr) -> 
        let env2 = TypeEnv.apply env sub3 in
        let schema = generalize env ty3 in
        let env3 = TypeEnv.extend env2 name schema in
        let* sub4, ty4 = helper env3 expr in
        let* sub5 = Subst.compose sub3 sub4 in
        return (sub5, ty4)
    in result
  | EDeclaration (name, expr1, expr2) as declaration  -> 
    match declaration with
    | EDeclaration (name, expr1, None) -> helper env expr1
    | EDeclaration (name, expr1, Some expr) ->
      let* sub1, ty1 = helper env expr1 in
      let env2 = TypeEnv.apply env sub1 in
      let schema = generalize env2 ty1 in
      let env2 = TypeEnv.extend env2 name schema in
      let* sub2, t2 = helper env2 expr in
      let* sub3 = Subst.compose sub1 sub2 in
      return (sub3, t2)
  in
  helper
;;

let infer_program env program =
  let rec helper acc = function
  | [] -> acc
  | hd :: tl ->
    let* acc_env, acc_names = acc in
    match hd with
    | EDeclaration (name, expr1, None) | ERecDeclaration (name, expr1, None) -> 
      let* sub, ty = infer_expr acc_env hd in
      let new_env = TypeEnv.extend acc_env name (Scheme(TVarSet.empty, ty)) in
      let update_name_list name names_list =
        (match List.mem name names_list with
        | true -> name :: List.filter ((<>) name) names_list
        | false -> name :: names_list) in
      let new_acc = return (new_env, update_name_list name acc_names) in
      helper new_acc tl
    | _ -> return (env, []) (* Unreachable *)
    in
    let* env, names = helper (return(env, [])) program in
    return (env, List.rev names)
;;


let run_expr_inferencer expr = Result.map snd (run (infer_expr TypeEnv.empty expr))
let run_program_inferencer program = run (infer_program TypeEnv.empty program)