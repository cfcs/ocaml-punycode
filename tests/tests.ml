open Rresult
open QCheck
open Astring

module Punycode_tests = struct
  let () = Printexc.record_backtrace false

  let rfc3492_vectors = [ (* lifted from RFC-3492 section 7.1*)
    ([ 0x0644; 0x064A; 0x0647; 0x0645; 0x0627; 0x0628; 0x062A; 0x0643; 0x0644;
     0x0645; 0x0648; 0x0634; 0x0639; 0x0631; 0x0628; 0x064A; 0x061F]
    , "egbpdaj6bu4bxfgehfvwxn");
    ([ 0x4ED6; 0x4EEC; 0x4E3A; 0x4EC0; 0x4E48; 0x4E0D; 0x8BF4; 0x4E2D; 0x6587]
    , "ihqwcrb4cv8a8dqg056pqjye");
    ([0x4ED6; 0x5011; 0x7232; 0x4EC0; 0x9EBD; 0x4E0D; 0x8AAA; 0x4E2D; 0x6587]
    , "ihqwctvzc91f659drss3x8bo0yb");
    ([0x0050; 0x0072; 0x006F; 0x010D; 0x0070; 0x0072; 0x006F; 0x0073; 0x0074;
      0x011B; 0x006E; 0x0065; 0x006D; 0x006C; 0x0075; 0x0076; 0x00ED; 0x010D;
      0x0065; 0x0073; 0x006B; 0x0079], "Proprostnemluvesky-uyb24dma41a");
    ([0x05DC; 0x05DE; 0x05D4; 0x05D4; 0x05DD; 0x05E4; 0x05E9; 0x05D5; 0x05D8;
      0x05DC; 0x05D0; 0x05DE; 0x05D3; 0x05D1; 0x05E8; 0x05D9; 0x05DD; 0x05E2;
      0x05D1; 0x05E8; 0x05D9; 0x05EA], "4dbcagdahymbxekheh6e0a7fei0b");
    ([0x092F; 0x0939; 0x0932; 0x094B; 0x0917; 0x0939; 0x093F; 0x0928; 0x094D;
      0x0926; 0x0940; 0x0915; 0x094D; 0x092F; 0x094B; 0x0902; 0x0928; 0x0939;
      0x0940; 0x0902; 0x092C; 0x094B; 0x0932; 0x0938; 0x0915; 0x0924; 0x0947;
      0x0939; 0x0948; 0x0902], "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd");
    ([0x306A; 0x305C; 0x307F; 0x3093; 0x306A; 0x65E5; 0x672C; 0x8A9E; 0x3092;
      0x8A71; 0x3057; 0x3066; 0x304F; 0x308C; 0x306A; 0x3044; 0x306E; 0x304B]
    , "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa");
    (* The one below is commented out due to being too long for DNS labels
    * ("xn--" + 69) > 63
    ([0xC138; 0xACC4; 0xC758; 0xBAA8; 0xB4E0; 0xC0AC; 0xB78C; 0xB4E4; 0xC774;
      0xD55C; 0xAD6D; 0xC5B4; 0xB97C; 0xC774; 0xD574; 0xD55C; 0xB2E4; 0xBA74;
      0xC5BC; 0xB9C8; 0xB098; 0xC88B; 0xC744; 0xAE4C]
    , "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5j"); *)
    ([0x043F; 0x043E; 0x0447; 0x0435; 0x043C; 0x0443; 0x0436; 0x0435; 0x043E;
      0x043D; 0x0438; 0x043D; 0x0435; 0x0433; 0x043E; 0x0432; 0x043E; 0x0440;
      0x044F; 0x0442; 0x043F; 0x043E; 0x0440; 0x0443; 0x0441; 0x0441; 0x043A;
      0x0438], "b1abfaaepdrnnbgefbaDotcwatmq2g4l");
    ([0x0050; 0x006F; 0x0072; 0x0071; 0x0075; 0x00E9; 0x006E; 0x006F; 0x0070;
      0x0075; 0x0065; 0x0064; 0x0065; 0x006E; 0x0073; 0x0069; 0x006D; 0x0070;
      0x006C; 0x0065; 0x006D; 0x0065; 0x006E; 0x0074; 0x0065; 0x0068; 0x0061;
      0x0062; 0x006C; 0x0061; 0x0072; 0x0065; 0x006E; 0x0045; 0x0073; 0x0070;
      0x0061; 0x00F1; 0x006F; 0x006C]
    , "PorqunopuedensimplementehablarenEspaol-fmd56a");
    ([0x0054; 0x1EA1; 0x0069; 0x0073; 0x0061; 0x006F; 0x0068; 0x1ECD; 0x006B;
      0x0068; 0x00F4; 0x006E; 0x0067; 0x0074; 0x0068; 0x1EC3; 0x0063; 0x0068;
      0x1EC9; 0x006E; 0x00F3; 0x0069; 0x0074; 0x0069; 0x1EBF; 0x006E; 0x0067;
      0x0056; 0x0069; 0x1EC7; 0x0074]
    , "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g");
    ([0x0033; 0x5E74; 0x0042; 0x7D44; 0x91D1; 0x516B; 0x5148; 0x751F]
    , "3B-ww4c5e180e575a65lsy2b");
    ([0x5B89; 0x5BA4; 0x5948; 0x7F8E; 0x6075; 0x002D; 0x0077; 0x0069; 0x0074;
      0x0068; 0x002D; 0x0053; 0x0055; 0x0050; 0x0045; 0x0052; 0x002D; 0x004D;
      0x004F; 0x004E; 0x004B; 0x0045; 0x0059; 0x0053]
    , "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n");
    ([0x0048; 0x0065; 0x006C; 0x006C; 0x006F; 0x002D; 0x0041; 0x006E; 0x006F;
      0x0074; 0x0068; 0x0065; 0x0072; 0x002D; 0x0057; 0x0061; 0x0079; 0x002D;
      0x305D; 0x308C; 0x305E; 0x308C; 0x306E; 0x5834; 0x6240]
    , "Hello-Another-Way--fc4qua05auwb3674vfr0b");
    ([0x3072; 0x3068; 0x3064; 0x5C4B; 0x6839; 0x306E; 0x4E0B; 0x0032]
    , "2-u9tlzr9756bt3uc0v");
    ([0x004D; 0x0061; 0x006A; 0x0069; 0x3067; 0x004B; 0x006F; 0x0069; 0x3059;
      0x308B; 0x0035; 0x79D2; 0x524D], "MajiKoi5-783gue6qz075azm5e");
    ([0x30D1; 0x30D5; 0x30A3; 0x30FC; 0x0064; 0x0065; 0x30EB; 0x30F3; 0x30D0]
    , "de-jg4avhby1noc0d");
    ([0x305D; 0x306E; 0x30B9; 0x30D4; 0x30FC; 0x30C9; 0x3067],
    "d9juau41awczczp")
  ]

  let utf8_buffer_len = 254 + 4

  let utf8_buffer = Bytes.make utf8_buffer_len '\x00'

  let to_utf8 codepoint_lst =
    let encoder = Uutf.encoder `UTF_8 `Manual in
    Uutf.Manual.dst encoder utf8_buffer 0 utf8_buffer_len ;
    List.iter
      (fun i ->
        if Uutf.Manual.dst_rem encoder > utf8_buffer_len - 255 then
          ignore @@ Uutf.encode encoder (`Uchar (Uchar.of_int i)))
      codepoint_lst ;
    ignore @@ Uutf.encode encoder `End ;
    Bytes.sub_string utf8_buffer 0
      (utf8_buffer_len - Uutf.Manual.dst_rem encoder)

  let test_rfc3492_vectors _ =
    List.fold_left
      (fun acc (codepoints, supposed_label) ->
        acc
        >>= fun () ->
        let supposed_label =
          (* fix incorrect casing in test vectors since Punycode is
             case-insensitive: *)
          let lst =
            String.cuts ~empty:true ~sep:"-" supposed_label |> List.rev
          in
          let last = List.hd lst |> String.Ascii.lowercase in
          List.rev (last :: List.tl lst) |> String.concat ~sep:"-"
        in
        let input_str = to_utf8 codepoints in
        Punycode.to_ascii input_str
        |> R.reword_error (fun _ -> R.msgf "failed to encode: %S" input_str)
        >>= fun encoded ->
        let without_xn = Astring.String.drop ~max:4 encoded in
        ( if without_xn = supposed_label then Ok ()
        else
          R.error_msgf "input: %S@,encoded: %S@,<>@,suppose: %S@ " input_str
            without_xn supposed_label )
        >>= fun () ->
        Punycode.to_utf8 encoded
        |> R.reword_error (fun _ -> R.msgf "failed to decode: %S" encoded)
        >>| fun decoded ->
        Alcotest.(check string) "decodes correctly" input_str decoded ;
        ())
      (Ok ()) rfc3492_vectors
    |> function Ok () -> () | Error (`Msg xxx) -> failwith xxx

  let assert_bool message actual =
    Alcotest.(check bool) message true actual

  let must_be_bool error_assertion input_str () =
    match Punycode.to_ascii input_str with
    | Ok encoded -> (
      match Punycode.to_utf8 encoded with
      | Ok decoded when decoded = input_str ->
          ()
      | _ ->
          assert_bool "failed to decode" error_assertion )
    | Error _ ->
        assert_bool "failed to encode" error_assertion

  let must_be_good str = must_be_bool false str

  let must_be_bad str = must_be_bool true str

  let test_decode_label _ =
    let decoded = Punycode.to_utf8 "xn--maana-pta.com" in
    match decoded with
    | Ok x when x = "mañana.com" ->
        assert_bool "itworked" true
    | Ok _ ->
        assert_bool "decodes mañana incorrectly" false
    | Error _ ->
        assert_bool "decode error for manana" false

  let test_encode_label _ =
    let encoded = Punycode.to_ascii "mañana.com" in
    match encoded with
    | Ok encoded when encoded = "xn--maana-pta.com" ->
        assert_bool "can encode manana" true
    | Ok bad ->
        assert_bool ("encodes manana incorrectly: " ^ bad) false
    | Error Punycode.Overflow ->
        assert_bool "encode overflow in manana" false
    | Error _ ->
        assert_bool "encode error in manana" false

  let test_regression_00 _ =
    (* this used to be accepted *)
    let input_str = "a\x13\xE5\xAF\xAA\xEA\x8C\xB4" in
    let open Punycode in
    match to_ascii input_str with
    | Ok _ ->
        assert_bool "incorrectly accepted low ascii for encoding" false
    | Error (Illegal_label (Label_contains_illegal_character label))
      when label = "xn--a\019-7k1d602x" ->
        () (* good. *)
    | Error err -> (
      match msg_of_encode_error err with
      | `Msg xxx ->
          assert_bool ("unexpected encoding error:" ^ xxx) false )

  let test_regression_01 = must_be_good "\xE9\x95\x97\xEE\xB1\xB8\xEF\x9F\xB4"

  let test_regression_02 = must_be_good "\xe2\x98\xad"

  let test_regression_03 =
    must_be_good
      "\xE8\x94\xAB\xC2\x9D\xEE\x86\xAF\xE4\x90\x99\xEF\xA5\xBF\xEF\x8F\xB7"

  let test_regression_04 =
    must_be_good
      "\xE8\xA4\x86\xD1\xB8\xE7\xB6\x8A\xE4\x9F\xB4\xE3\xB5\x9A\xE8\xBF\x92"

  let test_regression_05 = must_be_good "example.com."

  let test_regression_06 =
    (* generate "a.a.a.a" ... ".a.a." up to len 255 *)
    must_be_bad
    @@ String.v ~len:255 (fun i -> if i land 1 = 0 then 'a' else '.')

  let test_regression_07 =
    (* 253, ends with ".a" *)
    must_be_good
    @@ String.v ~len:253 (fun i -> if i land 1 = 0 then 'a' else '.')

  let test_regression_08 =
    (* 254 ends with ".bc" *)
    must_be_bad
    @@ String.v ~len:252 (fun i -> if i land 1 = 0 then 'a' else '.')
    ^ "bc"

  let test_regression_09 =
    (* 254, ends with ".b."*)
    must_be_good
    @@ String.v ~len:251 (fun i -> if i land 1 = 0 then 'a' else '.')
    ^ ".b."

  let test_regression_10 =
    must_be_good
    @@ String.v ~len:253 (* ends with '.' *) (fun i ->
           if i land 1 = 0 then 'a' else '.')
    ^ "."

  let test_regression_11 = must_be_bad ""

  let test_regression_12 = must_be_bad "."

  let utf8_string_of_size (inti : int Gen.t) : string QCheck.arbitrary =
    (* generates a valid utf-8 string containing random unicode characters *)
    let int_lst_t =
      Gen.list_size inti
        Gen.(
          int_bound 70
          >>= fun decide ->
          (* insert '.' approx every 70th *)
          if decide = 0 then return 0x2E
          else
            bool
            >>= function
            | false ->
                int_range 0 0xD7FF
            (* skip 0xD800..DFFF since they're reserved for UTF-16 encoding *)
            | true ->
                int_range 0xE000 0x10FFFF)
    in
    QCheck.make Gen.(int_lst_t >>= fun int_lst -> Gen.return (to_utf8 int_lst))

  let test_quickcheck_case input_str =
    let explain_fail explanation value value2 =
      ignore
      @@ failwith
           (strf "input str: %S failed: %S value1: %S value2: %S" input_str
              explanation value value2) ;
      false
    in
    let open Punycode in
    match Punycode.to_ascii input_str with
    (* Not expected to be OK: *)
    | Ok encoded when String.length encoded > 254 ->
        explain_fail "Mistakenly generated long domain" encoded ""
    | Ok encoded
      when String.length encoded = 254
           && encoded.[String.length encoded - 1] <> '.' ->
        explain_fail "domain len=254, but doesn't end in '.'" encoded ""
    | Ok "" ->
        explain_fail "encoded empty domain" "" ""
    (* Encoding went ok, try to decode: *)
    | Ok encoded -> (
      match Punycode.to_utf8 encoded with
      | Error err -> (
        match Punycode.msg_of_decode_error err with
        | `Msg err ->
            explain_fail ("failed to decode encoded: " ^ err) encoded "" )
      | Ok processed when processed <> input_str ->
          explain_fail "input_str <> decoded" processed encoded
      | Ok _processed ->
          true )
    (* Blacklist results from errors (regressions) *)
    | Error (Invalid_domain_name "invalid host name") when input_str = "" ->
        explain_fail "domain name too long, string length = 0" input_str ""
    (* Fail the test if the "illegal char" is actually okay: *)
    | Error
        (Illegal_label
          (Label_starts_with_illegal_character
            ('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_'))) ->
        explain_fail "Label starts with illegal character" "" ""
    (* Expected failures: *)
    | Error (Illegal_label (Illegal_label_size "")) ->
        true
    | Error (Illegal_label (Illegal_label_size label))
      when String.length label > 63 ->
        true
    | Error (Illegal_label (Illegal_label_size _))
    (* starts with '.'*)
      when String.is_prefix ~affix:"." input_str ->
        true
    (* contains empty label: *)
    | Error (Illegal_label (Illegal_label_size _))
      when String.is_infix ~affix:".." input_str ->
        true
    | Error (Illegal_label (Label_ends_with_hyphen _))
      when String.is_suffix ~affix:"-" input_str ->
        true
    | Error (Invalid_domain_name too_long) when String.length too_long >= 255
      ->
        true
    | Error (Invalid_domain_name too_long)
      when String.length too_long = 254 && too_long.[253] <> '.' ->
        true
    (* Accept error when the label does start with illegal char: *)
    | Error
        (Illegal_label
          (Label_starts_with_illegal_character
            ( '\x00' .. '\x2f'
            (* note that we don't accept '-' at beginning *)
            (* make space for numbers *)
            | '\x3a' .. '\x40'
            (* here comes uppercase letters *)
            | '\x5b' .. '\x5e'
            (* here comes '_' / 0x5f *)
            | '\x60'
            (* here comes lowercase letters *)
            | '\x7b' .. '\xff' ))) ->
        true
    (* Accept error when the input actually contains illegal chars: *)
    | Error (Illegal_label (Label_contains_illegal_character label))
      when not
           @@ String.for_all
                (function
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' ->
                      true
                  | _illegal_char ->
                      false)
                label ->
        true
    (* Unexpected errors result in failing the test: *)
    | Error err -> (
      match Punycode.msg_of_encode_error err with
      | `Msg str ->
          explain_fail str "" "UNEXPECTED" )

  let crowbar_uutf _ =
    let str_g : string Crowbar.gen =
      let open Crowbar in
      (* generates a valid utf-8 string containing random unicode characters *)
      let c_gen : int gen =
        choose
          [ range 0xD7FF
            (* skip 0xD800..DFFF since they're reserved for
                            UTF-16 encoding: *)
          ; map
              [range (*~min:0xE000 *) (0x10FFFF - 0xE000)]
              (fun x -> x + 0xE000) ]
      in
      map [list1 c_gen] to_utf8
    in
    Crowbar.add_test ~name:"Punycode.to_ascii" [str_g] (fun s ->
        try Crowbar.check (test_quickcheck_case s)
        with _ -> Crowbar.bad_test ())

  let test_alien_cases _ =
    Alcotest.(check(result (list string) reject))
      "punicode.js: xn-ZZZ -> u{7BA5}"
      (Punycode.to_utf8_list "xn--ZZZ")
      (Ok ["\231\174\165"]) ;
    Alcotest.(check(result (list string) reject))
      "simpleidn: xn--bonusaa-5bb1da"
      (Punycode.to_utf8_list "xn--bonusaa-5bb1da")
      (Ok ["bon\196\161usa\196\167\196\167a"]) ;
    Alcotest.(check(result (list string) reject))
      "simpleidn: xn--hxargifdar"
      (Punycode.to_utf8_list "xn--hxargifdar")
      (Ok ["\206\181\206\187\206\187\206\183\206\189\206\185\206\186\206\172"])

  let test_quickcheck_uutf _ =
    QCheck.Test.check_exn
    @@ QCheck.Test.make ~count:200_000 ~name:"quickcheck_uutf"
         (* 87: ~3 * 87 gives us max len: *)
         (utf8_string_of_size @@ Gen.int_range 1 87)
         (*bump this up for more errors!*)
         test_quickcheck_case

  let regressions : unit Alcotest.test_case list =
    [ ("00: fail on illegal char", `Quick, test_regression_00)
    ; ("01: complex unicode", `Quick, test_regression_01)
    ; ("02: simple unicode-only label", `Quick, test_regression_02)
    ; ("03: from issue #1", `Quick, test_regression_03)
    ; ("04: from issue #1", `Quick, test_regression_04)
    ; ("05: empty end label, from issue #2"
      , `Quick
      , test_regression_05 )
    ; ("06: FAIL domain a.a. .. .a. of 255 chars"
      , `Quick
      , test_regression_06 )
    ; ("07: OK domain a.a. .. .a of 253 chars"
      , `Quick
      , test_regression_07 )
    ; ("08: FAIL domain a.a. .. .bc of 254 chars"
      , `Quick
      , test_regression_08 )
    ; ("09: OK domain a.a. .. .b. of 254 chars"
      , `Quick
      , test_regression_09 )
    ; ("10: OK domain a.a. .. .a. of 253 chars"
      , `Quick
      , test_regression_10 )
    ; ("11: FAIL \"\"", `Quick, test_regression_11)
    ; ("12: FAIL \".\"", `Quick, test_regression_12) ]

  let unit_tests : unit Alcotest.test_case list =
    [ ("decode_label", `Quick, test_decode_label)
    ; ("encode_label", `Quick, test_encode_label)
    ; ("RFC-3492 test vectors", `Quick, test_rfc3492_vectors)
    ; ("testcases from other libraries", `Quick, test_alien_cases) ]

  let fuzzing : unit Alcotest.test_case list =
    [ ("quickcheck_uutf", `Slow, test_quickcheck_uutf)
    ; ("crowbar", `Slow, crowbar_uutf) ]
end

let tests : unit Alcotest.test list =
  [ ("unit tests", Punycode_tests.unit_tests)
  ; ("regressions", Punycode_tests.regressions)
  ; ("fuzzing", Punycode_tests.fuzzing) ]

let () = Alcotest.run "Punycode tests" tests
