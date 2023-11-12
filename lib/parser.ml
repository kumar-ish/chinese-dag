open Core
open Angstrom

module Definition = struct
  type t =
    { traditional : string [@printer fun fmt -> fprintf fmt "%s"]
    ; simplified : string [@printer fun fmt -> fprintf fmt "%s"]
    ; pronunciation : string
    ; meanings : string list
    }
  [@@deriving sexp]
end

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let pronunciation_start = '['
let pronunciation_end = ']'
let meanings_start = '/'
let meanings_end = '/'
let meanings_sep = ';'
let whitespace = take_while is_whitespace
let whitespace1 = take_while1 is_whitespace
let chinese_word = take_while1 (Fn.non is_whitespace)
let comment = char '#' >>| fun (_ : char) -> None

let pronunciation =
  char pronunciation_start *> take_while1 (fun c -> Char.(c <> pronunciation_end))
  <* char pronunciation_end
;;

let meaning =
  whitespace *> take_while1 (fun c -> Char.(c <> meanings_sep && c <> meanings_end))
;;

let meanings =
  char meanings_start *> sep_by (char meanings_sep) meaning <* char meanings_end
;;

(* *> sep_by (char ';') meaning <* char '/' *)

let definition =
  let open Angstrom.Let_syntax in
  let%bind traditional = whitespace *> chinese_word in
  let%bind simplified = whitespace1 *> chinese_word in
  let%bind pronunciation = whitespace1 *> pronunciation in
  let%map meanings = whitespace1 *> meanings in
  Some { Definition.traditional; simplified; pronunciation; meanings }
;;

let line_parser = comment <|> definition
let parse_chinese_line = parse_string ~consume:Prefix line_parser

let%expect_test "comment_line" =
  print_s
    [%message (parse_chinese_line "# CC-CEDICT" : (Definition.t option, string) Result.t)];
  [%expect {| ("parse_chinese_line \"# CC-CEDICT\"" (Ok ())) |}]
;;

let%expect_test "definition_line" =
  let parsed_expression =
    "AA制 AA制 [A A zhi4] /to split the bill; to go Dutch/"
    |> parse_chinese_line
    |> Result.ok_or_failwith
    |> Option.value_exn
  in
  print_endline [%string "traditional %{parsed_expression.traditional}"];
  print_endline [%string "simplified %{parsed_expression.simplified}"];
  print_s [%message (parsed_expression : Definition.t)];
  [%expect
    {|
    traditional AA制
    simplified AA制
    (parsed_expression
     ((traditional "AA\229\136\182") (simplified "AA\229\136\182")
      (pronunciation "A A zhi4") (meanings ("to split the bill" "to go Dutch")))) |}]
;;
