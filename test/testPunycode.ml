open OUnit2
open Punycode
open Rresult

let test_decode_label _ =
  let decoded = (Punycode.to_unicode "xn--maana-pta.com") in
  begin match decoded with
  | Ok x ->
    Printf.printf "outlen: %d out: %s\n" String.(length x) x
    ; assert_bool "itworked" true
  | Error _ ->
    assert_bool "decode error" false
  end
;;

let test_encode_label _ =
  let encoded = (Punycode.to_ascii "mañana.com") in
  begin match encoded with
	| Ok encoded ->
	   Printf.printf "encoded: %s" encoded;
	   assert_bool "can encode" true
	| Error _ ->
	   assert_bool "encode error" false
  end
;;

(* TODO write tests for:
   to_ascii
   to_unicode
*)

(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_hand" >::: [
    "decode_label" >:: test_decode_label;
    "encode_label" >:: test_encode_label;
  ]
