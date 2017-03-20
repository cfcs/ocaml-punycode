open OUnit2
open Rresult
open QCheck
open Astring

let test_decode_label _ =
  let decoded = (Punycode.to_unicode "xn--maana-pta.com") in
  begin match decoded with
  | Ok x ->
    assert_bool "itworked" true
  | Error _ ->
    assert_bool "decode error" false
  end
;;

let test_encode_label _ =
  let encoded = (Punycode.to_ascii "mañana.com") in
  begin match encoded with
	| Ok encoded ->
	   assert_bool "can encode" true
	| Error _ ->
	   assert_bool "encode error" false
  end
;;

let utf8_string_of_size (inti : int Gen.t) : string QCheck.arbitrary=
let int_lst_t =
    Gen.small_list
    Gen.(bool >>= function _ -> int_range 0 0xD7FF)
    (* TODO select from higher unicode plane too, need to look up the constants *)
in
let rec recursive_encoder enc = function
  | [] ->let _ = Uutf.encode enc `End in
         begin match Uutf.encoder_dst enc
         with `Buffer buf -> Gen.return @@ Buffer.contents buf
         end
  | cp::tl ->
       let uucp = if Uchar.is_valid cp then cp else 0x33 in
       begin match Uutf.encode enc (`Uchar uucp) with
       | `Ok | `Partial -> recursive_encoder enc tl
       end
in
Gen.(int_lst_t >>= fun int_lst ->
  recursive_encoder (Uutf.encoder `UTF_8 (`Buffer (Buffer.create 1))) int_lst
)
|> QCheck.make

let test_quickcheck_uutf _ =
  QCheck.Test.check_exn @@ QCheck.Test.make ~count:100000
  ~name:"quickcheck_uutf"
  (utf8_string_of_size Gen.int)
  (fun s ->
     begin match Punycode.to_ascii s with
     | Ok encoded ->
       begin match Punycode.to_unicode encoded with
       | Error _ -> false
       | Ok processed when processed <> s ->
           failwith ("s != processed: '"
                    ^String.Ascii.((escape s)
                      ^"' != "^(escape processed))
                     ^ " ("^ encoded^")"
           ); false
       | Ok processed -> true
       end

     (* Blacklist results from errors (regressions) *)
     | Error Domain_name_too_long too_long
       when String.length too_long = 0
       -> failwith "domain name too long, string length = 0";false
     | Error (Illegal_label (Label_starts_with_illegal_character
            ('0'..'9' | 'a'..'z' | 'A'..'Z' | '_')
        )) -> false

     (* Expected failures *)
     | Error Domain_name_too_long too_long
       when String.length too_long >= 253*4
       -> true
     | Error (Illegal_label (Illegal_label_size _))
       when String.empty = s -> true
     | Error (Illegal_label (Label_ends_with_hyphen _))
       when String.is_suffix ~affix:s "-"
       -> true
     | Error (Illegal_label (Label_starts_with_illegal_character
            ('\x00'..'\x2f' (* note that we don't accept '-' at beginning *)
              (* make space for numbers *)
            | '\x3a'..'\x40'
              (* here comes uppercase letters *)
            | '\x5b'..'\x5e'
              (* here comes '_' *)
            | '\x60'
              (* here comes lowercase letters *)
            | '\x7b'..'\xff')
        )) -> true
     | Error (Illegal_label (Label_contains_illegal_character label))
       when not @@ String.for_all
              (function 'a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_' -> true
              | _ -> false ) label
       -> true

     (* Unexpected errors result in failing the test *)
     | Error (Domain_name_too_long _) -> failwith "domain name too long"; false
     | Error (Illegal_label (Illegal_label_size _))
       -> failwith "illegal label SIZE"; false
     | Error (Illegal_label (Label_starts_with_illegal_character c)) -> failwith ("illegal label CHAR" ^ (String.of_char c)); false
     | Error (Illegal_label _) -> failwith "illegal label"; false
     | Error Malformed_utf8_input malformed -> false
     | Error Overflow -> false
   end
  )

let suite = "ts_hand" >::: [
    "decode_label" >:: test_decode_label;
    "encode_label" >:: test_encode_label;
    "quickcheck_uutf" >:: test_quickcheck_uutf;
  ]
