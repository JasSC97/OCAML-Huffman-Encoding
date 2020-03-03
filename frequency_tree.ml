open Base
include Lheap

(* this below module handles the frequency tree for our
huffman encoding *)
module FrequencyTree = struct

    (* this nested module handles and encapsulates the structure
    for a node *)
    module FrequencyNode = struct
        type t = 
            | Leaf
            | Node of (string * int) * t * t

        let compare tr1 tr2 =
            match tr1, tr2 with
                | Leaf, Leaf -> 0
                | Leaf, Node _ -> -1
                | Node _, Leaf -> 1
                | Node ((_, f'), _, _), Node ((_, f), _, _) -> f' - f
    end

    (* by definition for huffman coding, we require a priority queue ADT.
       we've opted to implement this using a leftist min heap (code provided by
       Albert Wei) 
    *)
    module MinHeap = Lheap.Make(FrequencyNode)

    type t = FrequencyNode.t

    (* test case for easy access *)
    let test = [("c", 2); ("b", 6); ("e", 7); ("_", 10);("d", 10); ("a", 11)]

    (* the below function will create an empty tree *)
    let empty = FrequencyNode.Leaf

    (* the below function checks if our frequency tree is empty *)
    let is_empty tr = 
        match tr with
        | FrequencyNode.Leaf -> true
        | _ -> false

    (* below is a private, tail recurisve helper function for 
       converting to a list 
    *)
    let rec lst_conv tr acc =
        match tr with
        | FrequencyNode.Leaf -> acc
        | FrequencyNode.Node ((s, f), l, r) -> 
            lst_conv l (lst_conv r ((s, f)::acc))

    (* public method to turn a tree to a list *)
    let to_list tr =
        lst_conv tr []

    (* the below 7 methods are helper functions for our overarching
       encoding algorithm. parts of them are used to:
       -convert a list of tuples (string * int) to a list of nodes for easy heapifying
       -inserting a new combination into a tree
       -retrieving the frequency of a current char
       -initializing our heap
       -processing our heap until we get a single tree node in our heap, and thus a finished huff tree
    *)
    let init_node ~str:s ~freq:f = FrequencyNode.Node ((s, f), FrequencyNode.Leaf, FrequencyNode.Leaf)

    (* processes inserting a new huffman node *)
    let insert_tree ~rt:rt ~lc:l ~rc:r = 
        match rt with
        | FrequencyNode.Leaf -> FrequencyNode.Leaf
        | FrequencyNode.Node (sf, Leaf, Leaf) -> FrequencyNode.Node (sf, l, r)
        | FrequencyNode.Node (_, _, _) -> failwith "insert_tree: already a combination"


    (* returns the frequency of a huffman node *)
    let get_char_freq tr =
        match tr with
        | FrequencyNode.Leaf -> None
        | FrequencyNode.Node ((s, f), _, _) -> Some (s, f)

    (* converts a list of (string * int) list to a node list *)
    let rec tuple_list_to_node_list lst =
        let open Base in
        List.fold_left ~f:(fun acc (s, f) -> (init_node s f)::acc) ~init:[] lst

    (* below creates new nodes from two smaller nodes for our huffman encoding tree *)
    let create_combination ~lc:l ~rc:r =
        match (get_char_freq l), (get_char_freq r) with
        | Some _, None -> failwith "create_combination: uninitialized right node"
        | None, Some _ -> failwith "create_combination: uninitialized left node"
        | None, None -> failwith "create_combination: uninitialized left and right nodes"
        | Some (l_str, l_freq), Some (r_str, r_freq) -> 
            insert_tree ~rt:(init_node ~str:(String.concat [l_str;r_str]) ~freq:(l_freq + r_freq)) ~lc:l ~rc:r;;


    let rec initialize_heap lst = MinHeap.heap_of_list lst
(* 
    below recursively takes from the top of the heap,
    applies the technique of creating a new node of
    two children nodes and appending it to our heap
    until our heap consists of just one huffman tree.
    we then return that huffman tree. *)
    let rec process_min_heap hp =
        let first_min = MinHeap.get_min hp in
        let hp = MinHeap.delete_min hp in
        if MinHeap.is_empty hp then
            first_min
        else
            let second_min = MinHeap.get_min hp in
            let hp = MinHeap.delete_min hp in
            let combo = create_combination first_min second_min in
            process_min_heap (MinHeap.insert hp combo)


    (* recursively goes down and generates the bit strings for our coding scheme. *)
    let rec get_bits tr num_bits =
        match tr with
        | FrequencyNode.Leaf -> []
        | FrequencyNode.Node ((c, f), FrequencyNode.Leaf, FrequencyNode.Leaf) -> (c, String.concat num_bits)::[]
        | FrequencyNode.Node (_, l, r) -> (get_bits l (num_bits @ ["0"])) @ (get_bits r (num_bits @ ["1"]))

    
    let hcode_of_list list =
        get_bits (process_min_heap (initialize_heap (tuple_list_to_node_list list))) []


    let rec length_finder freq_pairs coded_pairs acc =
        match freq_pairs, coded_pairs with
        | [], [] -> acc
        | [], lst -> failwith "length_finder: lists must contain same amount of elements"
        | lst, [] -> failwith "length_finder: lists must contain same amount of elements"
        | (_, f)::t', (_, c)::t ->
            length_finder t' t (acc + (f * String.length c))


    let encoded_length freq_list =
        let coded = hcode_of_list freq_list in
        let coded =
            List.sort ~compare:(fun(_, c')(_, c) ->
                (String.length c) - (String.length c')) coded in   
                let freq = List.sort ~compare: (fun (_, f') (_, f) -> f' - f) freq_list in
            length_finder freq coded 0



        
end