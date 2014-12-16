external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : string -> int -> char -> unit = "%string_safe_set"
external create : int -> string = "caml_create_string"
val copy : string -> string
val sub : string -> int -> int -> string
val fill : string -> int -> int -> char -> unit
val blit : string -> int -> string -> int -> int -> unit
val concat : string -> string list -> string
val iter : (char -> unit) -> string -> unit
val iteri : (int -> char -> unit) -> string -> unit
val map : (char -> char) -> string -> string
val trim : string -> string
val escaped : string -> string
val index : string -> char -> int
val rindex : string -> char -> int
val index_from : string -> int -> char -> int
val rindex_from : string -> int -> char -> int
val contains : string -> char -> bool
val contains_from : string -> int -> char -> bool
val rcontains_from : string -> int -> char -> bool
val uppercase : string -> string
val lowercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string
type t = string
val compare : t -> t -> int
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
  = "caml_blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
  = "caml_fill_string" "noalloc"
val make : int -> string
val int64_bytesize : int
val int32_bytesize : int
val int16_bytesize : int
val output_bytes : out_channel -> string -> unit
val set_binstring_of_int64 : int64 -> string -> unit
val set_binstring_of_float : float -> string -> unit
val set_binstring_of_int32 : int32 -> string -> unit
val set_binstring_of_int16 : int -> string -> unit
val set_binstring_of_int : int -> string -> unit
val output_int16 : out_channel -> int -> unit
val output_int32 : out_channel -> int32 -> unit
val output_int64 : out_channel -> int64 -> unit
