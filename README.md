# ocaml-punycode [![Build status](https://travis-ci.org/cfcs/ocaml-punycode.svg?branch=master)](https://travis-ci.org/cfcs/ocaml-punycode)

[RFC 3492: IDNA Punycode](https://tools.ietf.org/html/rfc3492) implementation.
It deals with full DNS domain names and their individual labels, and as such can
be used for encoding/decoding these, but it does not expose a generic Bootstring
or Punycode implementation.

Please report any issues or requests for additional features on the
[issue tracker](https://github.com/cfcs/ocaml-punycode/issues/).

### Interface:

```ocaml
val to_ascii : string -> (string, punycode_encode_error) result
(** [to_ascii domain_name] is the ASCII-only Punycode
    representation of [domain_name] with each label prefixed by "xn--",
    and where [domain_name] is a UTF-8-encoded DNS domain name (or label).
    An error is returned if the input is not valid UTF-8, or if the resulting
    encoded string would be an invalid domain name, for example if:
    - a non-Punycode label starts with a hyphen
    - a label is >= 64 ASCII characters or has a zero length
    - the total length is >= 256 ASCII characters.
 *)

val to_utf8 : string -> (string, punycode_decode_error) result
(** [to_utf8 punycode_domain] is the UTF-8 representation of [punycode_domain]
    where each label prefixed by "xn--" is decoded.
    The implementation strives to only accept valid domain names, see [to_ascii]
*)
```

### Examples

```ocaml
utop # Punycode.to_ascii "☫.example.ir";;
- : (string, Punycode.punycode_encode_error) result = Ok "xn--s4h.example.ir"

utop # Punycode.to_utf8 "xn--s4h.example.ir";;
- : (string, Punycode.punycode_decode_error) result =
Result.Ok "☫.example.ir"

utop # Punycode.to_ascii "n☢clear.disarmament.☮.example.com";;
- : (string, Punycode.punycode_encode_error) result =
Ok "xn--nclear-3b9c.disarmament.xn--v4h.example.com"

utop # Punycode.to_utf8 "xn--nclear-3b9c.disarmament.xn--v4h.example.com";;
- : (string, Punycode.punycode_decode_error) result =
Result.Ok "n☢clear.disarmament.☮.example.com"
```
