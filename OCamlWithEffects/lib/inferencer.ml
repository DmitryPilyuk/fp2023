open Type

(* TODO: decide where to put the output of types and errors *)
type error = [ `Occurs_check ]

let pp_error ppf : error -> _ =
  fun (e : error) ->
  match e with
  | `Occurs_check -> Format.fprintf ppf "{|Occurs check failed.|}"
;;

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

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (* module RList : sig
     val fold : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
     end *)

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
  val find : int -> t -> typ option
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
    return Base.Map.singleton (module Base.Int) k v
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

  (* TODO: unify, extend, compose, compose_all *)
end
