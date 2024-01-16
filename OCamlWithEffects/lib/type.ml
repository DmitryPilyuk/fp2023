(* todo: добавить шапку *)
(* todo: добавить комменты к типам *)
(* TODO: decide whether we implement char/string or not *)

type type_var = int

type primitive_type =
  | Int
  | Bool
  | Unit
  | Char
  | String
[@@deriving eq, show { with_path = false }]

type typ =
  | TPrim of primitive_type
  | TVar of type_var
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TEffect of typ
;;

module TVarSet = Stdlib.Set.Make (Int)

type scheme = Scheme of TVarSet.t * typ

(* Constructors for types *)
let tint = TPrim Int
let tbool = TPrim Bool
let tunit = TPrim Unit
let tchar = TPrim Char
let tstring = TPrim String
let tvar x = TVar x
let tarrow left right = TArr (left, right)
let ttuple x = TTuple x
let tlist x = TList x
let teffect x = TEffect x
let (@->) = tarrow