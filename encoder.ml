open Base
open Core
open Stdio
open Frequency_tree

(* this class is our main program, it will take input and provide output
for a huffman encoding *)

(* Below is a helper method to receive input, as lines, from the user. *)
let rec receive_input () =
    match In_channel.input_line stdin with
    | None -> []
    | Some line ->
            line::(receive_input ())

(* Below processes lines passed into it. It will not handle invalid output and
   will crash if provided with it. *)
let rec process_frequencies lines acc =
    match lines with
    | [] -> acc
    | line::t -> 
        let (s, f) = Scanf.sscanf line "%s %d" (fun a b -> (a, b)) in
        process_frequencies t ((s,f)::acc)


(* Below takes in the user's input *)
let user_lines = receive_input ()

(* Below handles turning our input into a form for our frequency tree *)
let char_freq_list = process_frequencies user_lines []
let codes = Frequency_tree.FrequencyTree.hcode_of_list char_freq_list
let codes = List.sort ~compare:(fun (s', _) (s, _) -> String.compare s' s) codes
let () = printf "\n\n"

(* Below does all of the printing for the codes *)
let () = List.iter codes ~f:(fun (w, f) -> printf "%s: %s\n" w f) 
let () = printf "Length: %d" (Frequency_tree.FrequencyTree.encoded_length char_freq_list)
let () = printf "\n\n"
let () = Out_channel.flush stdout
