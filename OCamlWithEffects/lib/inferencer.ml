(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Type

(* TODO: decide where to put the output of types and errors *)
type error =
  [ `Occurs_check
  | `Unification_failed of typ * typ
  | `No_variable of string
  ]

let rec pp_type ppf (typ : typ) =
  match typ with
  | TPrim x ->
    (match x with
     | Int -> Format.fprintf ppf "int"
     | Bool -> Format.fprintf ppf "bool"
     | Unit -> Format.fprintf ppf "unit"
     | Char -> Format.fprintf ppf "char"
     | String -> Format.fprintf ppf "string")
  | TVar x -> Format.fprintf ppf "%s" @@ Char.escaped (Char.chr (x + 97))
  | TArr (l, r) ->
    (match l, r with
     | TArr _, _ -> Format.fprintf ppf "(%a) -> %a" pp_type l pp_type r
     | _, _ -> Format.fprintf ppf "%a -> %a" pp_type l pp_type r)
  | TList x ->
    (match x with
     | TArr _ -> Format.fprintf ppf "(%a) list" pp_type x
     | _ -> Format.fprintf ppf "%a list" pp_type x)
  | TEffect x ->
    (match x with
     | TArr _ -> Format.fprintf ppf "(%a) effect" pp_type x
     | _ -> Format.fprintf ppf "%a effect" pp_type x)
  | TTuple tup_list ->
    Format.fprintf
      ppf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun _ _ -> Format.fprintf ppf " * ")
         (fun ppf typ -> pp_type ppf typ))
      tup_list
;;

let pp_error ppf : error -> _ =
  fun (e : error) ->
  match e with
  | `Occurs_check -> Format.fprintf ppf "{|Occurs check failed.|}"
  | `Unification_failed (typ1, typ2) ->
    Format.fprintf ppf "{|Unification failed: type is |}";
    pp_type ppf typ1;
    Format.fprintf ppf "{|, but expexted |}";
    pp_type ppf typ2
  | `No_variable name -> Format.fprintf ppf "{|No such variable: %s|}" name
;;

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
    (* | TPrim _, TPrim _ -> fail (`Unification_failed(l, r)) - вроде охватывается последним случаем *)
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
      Base.Map.update env key ~f:(fun _ -> schema)
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
  | None -> fail (`No_variable var_name)
;;