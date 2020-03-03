
module FrequencyTree : sig
    module FrequencyNode : sig
        type t
        val compare : t -> t -> int
    end
    type t
    val empty : t
    val is_empty : t -> bool
    val to_list : t -> (string * int) list
    val init_node : str:string -> freq:int -> t
    val hcode_of_list : (string * int) list -> (string * string) list
    val encoded_length : (string * int) list -> int
end
