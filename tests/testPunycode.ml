open OUnit2
open Rresult
open QCheck
open Astring

(*  let b = Buffer.create 1 in let encoder = Uutf.encoder `UTF_8 (`Buffer b) in List.iter (fun i -> assert (Uutf.encode encoder (`Uchar i) <> `Partial);()) [0xe9;0x2020]; Uutf.encode encoder `End; Buffer.to_bytes b;; *)
let rfc3492_tests = [
([ 0x0644; 0x064A; 0x0647; 0x0645; 0x0627; 0x0628; 0x062A; 0x0643; 0x0644; 0x0645; 0x0648; 0x0634; 0x0639; 0x0631; 0x0628; 0x064A; 0x061F], "egbpdaj6bu4bxfgehfvwxn");
([0x4ED6; 0x4EEC; 0x4E3A; 0x4EC0; 0x4E48; 0x4E0D; 0x8BF4; 0x4E2D; 0x6587], "ihqwcrb4cv8a8dqg056pqjye");
([0x4ED6; 0x5011; 0x7232; 0x4EC0; 0x9EBD; 0x4E0D; 0x8AAA; 0x4E2D; 0x6587], "ihqwctvzc91f659drss3x8bo0yb");
([0x0050; 0x0072; 0x006F; 0x010D; 0x0070; 0x0072; 0x006F; 0x0073; 0x0074; 0x011B; 0x006E; 0x0065; 0x006D; 0x006C; 0x0075; 0x0076; 0x00ED; 0x010D; 0x0065; 0x0073; 0x006B; 0x0079], "Proprostnemluvesky-uyb24dma41a");
([0x05DC; 0x05DE; 0x05D4; 0x05D4; 0x05DD; 0x05E4; 0x05E9; 0x05D5; 0x05D8; 0x05DC; 0x05D0; 0x05DE; 0x05D3; 0x05D1; 0x05E8; 0x05D9; 0x05DD; 0x05E2; 0x05D1; 0x05E8; 0x05D9; 0x05EA], "4dbcagdahymbxekheh6e0a7fei0b");
([0x092F; 0x0939; 0x0932; 0x094B; 0x0917; 0x0939; 0x093F; 0x0928; 0x094D; 0x0926; 0x0940; 0x0915; 0x094D; 0x092F; 0x094B; 0x0902; 0x0928; 0x0939; 0x0940; 0x0902; 0x092C; 0x094B; 0x0932; 0x0938; 0x0915; 0x0924; 0x0947; 0x0939; 0x0948; 0x0902], "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd");
([0x306A; 0x305C; 0x307F; 0x3093; 0x306A; 0x65E5; 0x672C; 0x8A9E; 0x3092; 0x8A71; 0x3057; 0x3066; 0x304F; 0x308C; 0x306A; 0x3044; 0x306E; 0x304B], "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa");
([0xC138; 0xACC4; 0xC758; 0xBAA8; 0xB4E0; 0xC0AC; 0xB78C; 0xB4E4; 0xC774; 0xD55C; 0xAD6D; 0xC5B4; 0xB97C; 0xC774; 0xD574; 0xD55C; 0xB2E4; 0xBA74; 0xC5BC; 0xB9C8; 0xB098; 0xC88B; 0xC744; 0xAE4C], "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5j");
([0x043F; 0x043E; 0x0447; 0x0435; 0x043C; 0x0443; 0x0436; 0x0435; 0x043E; 0x043D; 0x0438; 0x043D; 0x0435; 0x0433; 0x043E; 0x0432; 0x043E; 0x0440; 0x044F; 0x0442; 0x043F; 0x043E; 0x0440; 0x0443; 0x0441; 0x0441; 0x043A; 0x0438], "b1abfaaepdrnnbgefbaDotcwatmq2g4l");
([0x0050; 0x006F; 0x0072; 0x0071; 0x0075; 0x00E9; 0x006E; 0x006F; 0x0070; 0x0075; 0x0065; 0x0064; 0x0065; 0x006E; 0x0073; 0x0069; 0x006D; 0x0070; 0x006C; 0x0065; 0x006D; 0x0065; 0x006E; 0x0074; 0x0065; 0x0068; 0x0061; 0x0062; 0x006C; 0x0061; 0x0072; 0x0065; 0x006E; 0x0045; 0x0073; 0x0070; 0x0061; 0x00F1; 0x006F; 0x006C], "PorqunopuedensimplementehablarenEspaol-fmd56a");
([0x0054; 0x1EA1; 0x0069; 0x0073; 0x0061; 0x006F; 0x0068; 0x1ECD; 0x006B; 0x0068; 0x00F4; 0x006E; 0x0067; 0x0074; 0x0068; 0x1EC3; 0x0063; 0x0068; 0x1EC9; 0x006E; 0x00F3; 0x0069; 0x0074; 0x0069; 0x1EBF; 0x006E; 0x0067; 0x0056; 0x0069; 0x1EC7; 0x0074], "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g");
([0x0033; 0x5E74; 0x0042; 0x7D44; 0x91D1; 0x516B; 0x5148; 0x751F], "3B-ww4c5e180e575a65lsy2b");
([0x5B89; 0x5BA4; 0x5948; 0x7F8E; 0x6075; 0x002D; 0x0077; 0x0069; 0x0074; 0x0068; 0x002D; 0x0053; 0x0055; 0x0050; 0x0045; 0x0052; 0x002D; 0x004D; 0x004F; 0x004E; 0x004B; 0x0045; 0x0059; 0x0053], "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n");
([0x0048; 0x0065; 0x006C; 0x006C; 0x006F; 0x002D; 0x0041; 0x006E; 0x006F; 0x0074; 0x0068; 0x0065; 0x0072; 0x002D; 0x0057; 0x0061; 0x0079; 0x002D; 0x305D; 0x308C; 0x305E; 0x308C; 0x306E; 0x5834; 0x6240], "Hello-Another-Way--fc4qua05auwb3674vfr0b");
([0x3072; 0x3068; 0x3064; 0x5C4B; 0x6839; 0x306E; 0x4E0B; 0x0032], "2-u9tlzr9756bt3uc0v");
([0x004D; 0x0061; 0x006A; 0x0069; 0x3067; 0x004B; 0x006F; 0x0069; 0x3059; 0x308B; 0x0035; 0x79D2; 0x524D], "MajiKoi5-783gue6qz075azm5e");
([0x30D1; 0x30D5; 0x30A3; 0x30FC; 0x0064; 0x0065; 0x30EB; 0x30F3; 0x30D0], "de-jg4avhby1noc0d");
([0x305D; 0x306E; 0x30B9; 0x30D4; 0x30FC; 0x30C9; 0x3067]	,"d9juau41awczczp")
]

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
    Gen.(bool >>= function
                  | false -> int_range 0      0xD7FF
                  | true  -> int_range 0xE000 0xFFFF)
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
