type punycode_encode_error =
| Malformed_input of string
| Overflow

type punycode_decode_error =
| Illegal_basic_codepoints
| Overflow_error

val to_ascii : string -> (string, punycode_encode_error) Rresult.result
val to_unicode : string -> (string, punycode_decode_error) Rresult.result
