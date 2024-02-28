  $ dune exec demo << EOF
  > let rec fold_left f acc l = 
  > match l with
  >  | [] -> acc
  >  | h :: tl -> fold_left f (f acc h) tl
  > ;;
  > let reverse l = fold_left (fun acc h -> h :: acc) [] l
  > 
  > let lst = [1; 2; 3; 4; 5]
  > let res = reverse lst
  > EOF
  val fold_left : ('l -> 'f -> 'l) -> 'l -> 'f list -> 'l = <fun>
  val reverse : 't list -> 't list = <fun>
  val lst : int list = [1; 2; 3; 4; 5]
  val res : int list = [5; 4; 3; 2; 1]
