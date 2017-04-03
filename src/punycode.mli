(** Punycode (RFC 3492) utility library
*)

type illegal_ascii_label =
  | Illegal_label_size of string
  | Label_contains_illegal_character of string
  | Label_starts_with_illegal_character of char
  | Label_ends_with_hyphen of string

type punycode_decode_error =
  | Illegal_basic_codepoints
  | Overflow_error
  | Domain_name_too_long of string
  | Illegal_label of illegal_ascii_label

type punycode_encode_error =
  | Malformed_utf8_input of string
  | Overflow
  | Domain_name_too_long of string
  | Illegal_label of illegal_ascii_label

val to_ascii : string -> (string, punycode_encode_error) Rresult.result
(** Converts a UTF-8-encoded string to a Punycode string prefixed with "xn--" *)

val to_unicode : string -> (string, punycode_decode_error) Rresult.result
(** Converts a Punycode-encoded string to UTF-8 *)
