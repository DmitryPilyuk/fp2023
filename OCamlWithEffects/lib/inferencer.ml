open Type

(* TODO: decide where to put the output of types and errors *)
type error = [
  `Occurs_check
]

let pp_error ppf : error -> _ = fun (e: error) ->
  match e with
  | `Occurs_check -> Format.fprintf ppf "{|Occurs check failed.|}"
;;

let rec pp_type ppf = fun (typ: typ) ->
  match typ with
  | TPrim x -> 
    (match x with
    | Int -> Format.fprintf ppf "int"
    | Bool -> Format.fprintf ppf "bool"
    | Unit -> Format.fprintf ppf "unit"
    | Char -> Format.fprintf ppf "char"
    | String -> Format.fprintf ppf "string")
;;

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (* module RList : sig
    val fold : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end *)

  module RMap : sig
    val fold_left
      : ('a, 'b, 'c) Base.Map.t 
      -> init:'d t 
      -> f:('a -> 'b -> 'd -> 'd t) 
      -> 'd t
  end

  val fresh: int t

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
  let fresh : int t = fun last -> last+1, Result.Ok last

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
