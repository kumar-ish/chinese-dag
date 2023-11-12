module Definition : sig
  type t =
    { traditional : string
    ; simplified : string
    ; pronunciation : string
    ; meanings : string list
    }
  [@@deriving sexp]
end

val parse_chinese_line : string -> (Definition.t option, string) result
