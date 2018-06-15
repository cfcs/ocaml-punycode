# ocaml-punycode [![Build status](https://travis-ci.org/cfcs/ocaml-punycode.svg?branch=master)](https://travis-ci.org/cfcs/ocaml-punycode)

RFC 3492: IDNA Punycode implementation.
It deals with full DNS domain names and their individual labels, and as such can be used for encoding/decoding these,
but it does not expose a generic Bootstring implementation.

### Interface:

```ocaml
val to_ascii : string -> (string, punycode_encode_error) result
(** Converts a UTF-8-encoded string to a Punycode string prefixed with "xn--" *)

val to_unicode : string -> (string, punycode_decode_error) result
(** Converts a Punycode-encoded string to UTF-8 *)
```

### Examples

```ocaml
utop # Punycode.to_ascii "☫.example.ir";;
- : (string, Punycode.punycode_encode_error) result = Ok "xn--s4h.example.ir"

utop # Punycode.to_utf8 "xn--s4h.example.ir";;
- : (string, Punycode.punycode_decode_error) result =
Result.Ok "☫.example.ir"

utop # Punycode.to_ascii "☢.nuclear.disarmament.☮.example.com";;
- : (string, Punycode.punycode_encode_error) result =
Ok "xn--j4h.nuclear.disarmament.xn--v4h.example.com"

utop # Punycode.to_utf8 "xn--j4h.nuclear.disarmament.xn--v4h.example.com";;
- : (string, Punycode.punycode_decode_error) result =
Result.Ok "☢.nuclear.disarmament.☮.example.com"
```
