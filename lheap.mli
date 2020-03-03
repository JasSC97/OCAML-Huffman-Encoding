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

module Make(Ord: OrderedType) : S with type elt := Ord.t