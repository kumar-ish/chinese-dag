open! Core

let () =
  In_channel.read_lines "cedict_ts.truncated.u8"
  |> List.map ~f:Chinese_dag.Parser.parse_chinese_line
  |> List.iter ~f:(fun x ->
         print_s
           [%message (Result.ok_or_failwith x : Chinese_dag.Parser.Definition.t option)])
;;
