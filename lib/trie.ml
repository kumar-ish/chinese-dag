open Core

type t =
  { character : Uchar.t
  ; mutable is_end : bool
  ; children : (Uchar.t, t) Hashtbl.t
  }

let create_node character =
  let children = Hashtbl.create (module Uchar) in
  let is_end = false in
  { character; is_end; children }
;;

let construct_trie _words =
  let root_node = create_node (Uchar.of_char ' ') in
  let uchar_folder current_node _ = function
    | `Uchar u ->
      (match Hashtbl.find current_node.children u with
      | Some a -> a
      | None ->
        let new_node = create_node u in
        let (_ : [ `Duplicate | `Ok ]) =
          Hashtbl.add current_node.children ~key:u ~data:new_node
        in
        new_node)
    | `Malformed _ -> failwith "malformed input"
  in
  List.iter _words ~f:(fun s ->
      let bottom_node = Uutf.String.fold_utf_8 uchar_folder root_node s in
      bottom_node.is_end <- true);
  root_node
;;

let find_in_trie trie word =
  let uchar_folder a _ char =
    match a with
    | None -> None
    | Some a ->
      (match char with
      | `Uchar u ->
        (match Hashtbl.find a.children u with
        | Some child -> Some child
        | None -> None)
      | `Malformed _ -> None)
  in
  match Uutf.String.fold_utf_8 uchar_folder (Some trie) word with
  | Some _ -> true
  | None -> false
;;

let create_string word =
  let b = Buffer.create 4 in
  List.iter word ~f:(Uutf.Buffer.add_utf_8 b);
  Buffer.contents b
;;

(* Greedily finds longest-matching Chinese words from trie dictionary *)
let find_longest_matching_symbols root_node text =
  let uchar_folder ls _ = function
    | `Uchar u -> u :: ls
    | `Malformed _ -> ls
  in
  let uchar_list = List.rev @@ Uutf.String.fold_utf_8 uchar_folder [] text in
  let rec find_longest_matching_word node (longest_match, acc) = function
    | [] -> longest_match, acc
    | uchar :: rest ->
      (match Hashtbl.find node.children uchar with
      | None -> longest_match, acc
      | Some child ->
        let longest_match, acc' =
          let string_so_far = acc @ [ uchar ] in
          if child.is_end
          then Some string_so_far, string_so_far
          else longest_match, string_so_far
        in
        find_longest_matching_word child (longest_match, acc') rest)
  in
  let rec find_matching_symbols = function
    | [] -> []
    | _ as uchar_list ->
      let longest_match, _acc =
        find_longest_matching_word root_node (None, []) uchar_list
      in
      (match longest_match with
      | None -> find_matching_symbols (List.tl_exn uchar_list)
      | Some match_ ->
        match_ :: find_matching_symbols (List.drop uchar_list (List.length match_)))
  in
  find_matching_symbols uchar_list |> List.map ~f:create_string
;;

module Statistics = struct
  type t = (string, int) Hashtbl.t

  let create _ = Hashtbl.create (module String)

  let update_count t key =
    Hashtbl.update t key ~f:(function
        | None -> 1
        | Some count -> count + 1)
  ;;
end

let gather_statistics words =
  let table = Statistics.create () in
  List.iter words ~f:(fun w -> Statistics.update_count table w);
  table
;;

(*
            我T
的T  们T  人T  国T  去T   这F
                      儿T  个F
                          人T
*)

(* whitespace *)
(* chinese_word *)
(* chinese_word *)
(* pronunciation *)
(* meaning *)
