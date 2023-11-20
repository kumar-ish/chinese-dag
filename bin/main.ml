open! Core

let get_s (d : Chinese_dag.Parser.Definition.t) = d.simplified

let global_trie =
  In_channel.read_lines "cedict_ts.u8"
  |> List.filter_map ~f:(fun x ->
         Chinese_dag.Parser.parse_chinese_line x |> Result.ok_or_failwith)
  |> List.map ~f:get_s
  |> Chinese_dag.Trie.construct_trie
;;

let () =
  let matches =
    Chinese_dag.Trie.find_longest_matching_symbols
      global_trie
      "1、打破关住自己的门\n\n\
      \      一个木匠做得一手好门。他给自己家做了一扇门，他认为这门用料实在，做工精良，一定会经久耐用。 \
       过了一段时间，门的钉子锈了，掉下一块板，木匠找出一颗钉子补上，门又完好如初。不久又掉了一颗钉子，木匠就换上一颗钉子。后来，又有一块板坏了，木匠就又找出一块板换上。再后来，门闩坏了，木匠又换了一个门闩„„ \
       若干年后，这扇门虽经无数次破损，但经过木匠的精心修理，仍坚固耐用。木匠对此甚是自豪：多亏有了这门手艺，不然门坏了还不知如何是好。 \
       忽然有一天，邻居对他说：“你是木匠，你看看你家这门！”木匠仔细一看，才发觉邻居家的门一扇扇样式新颖、质地优良，而自己家的门又老又破，满是补丁。木匠明白了，是自己的这种门手艺阻碍了自家“门”的发展。\n\
      \      "
  in
  let table = Chinese_dag.Trie.gather_statistics matches in
  Hashtbl.to_alist table
  |> List.sort ~compare:(fun (_, a) (_, b) -> b - a)
  |> List.iter ~f:(fun (_key, _data) -> printf "%s %d" _key _data)
;;

(* |> List.iter ~f:(fun x ->
         print_s
           [%message (Result.ok_or_failwith x : Chinese_dag.Parser.Definition.t option)]) *)
