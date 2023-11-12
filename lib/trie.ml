module TrieNode = struct
  type t =
    { character : Uchar.t
    ; is_end : bool
    ; children : (Uchar.t, t) Hashtbl.t
    }
end

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

(* Aho–Corasick algorithm *)
