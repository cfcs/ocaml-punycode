# ocaml-punycode [![Build status](https://travis-ci.org/cfcs/ocaml-punycode.svg?branch=master)](https://travis-ci.org/cfcs/ocaml-punycode)

[RFC 3492: IDNA Punycode](https://tools.ietf.org/html/rfc3492) implementation.
It deals with full DNS domain names and their individual labels, and as such can
be used for encoding/decoding these, but it does not expose a generic Bootstring
or Punycode implementation.

Domain name / label validation is provided by
[Hannes Mehnert](https://github.com/hannesm/)'s
[`domain-name` library](https://github.com/hannesm/domain-name)

String handling and Unicode support comes courtesy of
[David Bünzli](https://erratique.ch/)'s
[`astring`](https://github.com/dbuenzli/astring) and
[`uutf`](https://github.com/dbuenzli/uutf) libraries.

Please report any issues or requests for additional features on the
[issue tracker](https://github.com/cfcs/ocaml-punycode/issues/).

### Interface:

```ocaml

(** {1 Error messages} *)


type illegal_ascii_label =
  | Illegal_label_size of string
  | Label_contains_illegal_character of string
  | Label_starts_with_illegal_character of char
  | Label_ends_with_hyphen of string

type punycode_decode_error =
  | Overflow_error
  | Invalid_domain_name of string
  | Illegal_label of illegal_ascii_label

type punycode_encode_error =
  | Malformed_utf8_input of string
  | Overflow
  | Invalid_domain_name of string
  | Illegal_label of illegal_ascii_label

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


(** {1:unicode2punycode Unicode -> Punycode}*)


val to_encoded_domain_name : string ->
  (Domain_name.t, punycode_encode_error) Rresult.result
(** [to_encoded_domain_name domain_name] is the ASCII-only Punycode
    representation of [domain_name] with each label prefixed by ["xn--"],
    and where [domain_name] is a UTF-8-encoded DNS domain name (or label).
    An error is returned if the input is not valid UTF-8, or if the resulting
    encoded string would be an invalid domain name, for example if:
    - a non-Punycode label starts with a hyphen
    - a label is >= 64 ASCII characters or has a zero length
    - the total length is >= 255 ASCII characters including trailing ['.']
      (if not present it will be assumed).
    See {Domain_name.of_strings} for more information regarding the
    produced value and its validation.
*)

val to_ascii : string -> (string, punycode_encode_error) Rresult.result
(** [to_ascii domain_name] is the ASCII-only Punycode
    representation of [domain_name] with each label prefixed by ["xn--"],
    joined by ['.']
    See {!to_encoded_domain_name} for more information.
*)


(** {1:punycode2unicode Punycode -> Unicode} *)


val of_domain_name : Domain_name.t ->
  (Uchar.t list list, punycode_decode_error) Rresult.result
(** [of_domain_name domain] is [domain] decoded to a list of [Uchar.t] elements
    for each label in the domain name with each label prefixed by ["xn--"]
    decoded using the Punycode algorithm.*)

val to_utf8_list : string -> (string list, punycode_decode_error) Rresult.result
(** [to_utf8_list domain] is the UTF-8 representation of [domain]
    where each label prefixed by ["xn--"] is decoded using the
    Punycode algorithm.
    The implementation strives to only accept valid domain names,
    see {!to_ascii}.
    If [domain] is a FQDN (has a trailing ['.']), this is ignored.
*)

val to_utf8 : string -> (string, punycode_decode_error) Rresult.result
(** [to_utf8 domain] is {!to_utf8_list} concatenated with dots.
    Contrary to {!to_utf8_list}, If [domain] is a FQDN (has a trailing ['.']),
    the decoded string will also have a trailing ['.'].
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

### Run tests

This project organizes its tests using Alcotest, and this dune alias runs them:

```shell
dune runtest --force --no-buffer
```

Using `--no-buffer` gets you colors in output.
