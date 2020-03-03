module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t
  val insert : t -> elt -> t
  val is_empty : t -> bool
  val heap_of_list : elt list -> t
  val get_min : t -> elt
  val delete_min : t -> t
end


module Make(Ord: OrderedType) = struct

    (* E is the empty node, used for leaves
    T is the node holding its own rank, its data, and its two subtrees *)
    type elt = Ord.t
    type t = E | T of int * Ord.t * t * t

    let rank = function
        | E -> 0
        | T (r, _, _, _) -> r

    let rec merge t1 t2 =
        match t1, t2 with
        | E, t | t, E -> t
        | T (_, x1, _, _), T (_, x2, _, _) when Ord.compare x2 x1 < 0 -> merge t2 t1
        | T (_, x, l, r), t -> 
            let r' = merge r t in
            if rank r' <= rank l then T (rank r' + 1, x, l, r')
            else T (rank l + 1, x, r', l)

    let insert tr x = merge tr (T (1, x, E, E))

    let heap_of_list l =
        let open Base in
        List.fold_left ~f:(fun t x -> insert t x) ~init:E l

    let is_empty tr =
        match tr with
        | E -> true
        | T _ -> false

    let get_min = function
        | E -> failwith "get_min: empty tree"
        | T (_, x, _, _) -> x

    let delete_min = function
        | E -> failwith "delete min: empty tree"
        | T (_, _, l, r) -> merge l r

end