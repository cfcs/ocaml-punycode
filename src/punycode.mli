(** Punycode (RFC 3492) utility library
*)

type illegal_ascii_label =
  | Illegal_label_size of string
  | Label_contains_illegal_character of string
  | Label_starts_with_illegal_character of char
  | Label_ends_with_hyphen of string

type punycode_decode_error =
  | Overflow_error
  | Domain_name_too_long of string
  | Illegal_label of illegal_ascii_label

type punycode_encode_error =
  | Malformed_utf8_input of string
  | Overflow
  | Domain_name_too_long of string
  | Illegal_label of illegal_ascii_label

val to_ascii : string -> (string, punycode_encode_error) result
(** Converts a UTF-8-encoded string to a Punycode string prefixed with "xn--" *)

val to_utf8 : string -> (string, punycode_decode_error) result
(** Converts a Punycode-encoded string (prefixed with "xn--" to UTF-8 *)


val msg_of_encode_error : punycode_encode_error -> [> `Msg of string]
(** [msg_of_encode_error err] is [err] transcribed to a [Rresult.R.msg],
    making it easier to display the error and use it with the Rresult monad.
    Can be used like this:
    [R.reword_error msg_of_encode_error (Punycode.to_ascii "example.com")]
*)

val msg_of_decode_error : punycode_decode_error -> [> `Msg of string]
(** [msg_of_decode_error err] is [err] transcribed to a [Rresult.R.msg],
    making it easier to display the error and use it with the Rresult monad.
    Can be used like this:
    [R.reword_error msg_of_decode_error (Punycode.to_utf8 "example.com")]
*)
