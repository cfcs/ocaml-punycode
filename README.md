# ocaml-punycode [![Build status](https://travis-ci.org/cfcs/ocaml-punycode.svg?branch=master)](https://travis-ci.org/cfcs/ocaml-punycode)

RFC 3492: IDNA Punycode implementation.

```ocaml
val to_ascii : string -> (string, punycode_encode_error) result
(** Converts a UTF-8-encoded string to a Punycode string prefixed with "xn--" *)

val to_unicode : string -> (string, punycode_decode_error) result
(** Converts a Punycode-encoded string to UTF-8 *)
```
