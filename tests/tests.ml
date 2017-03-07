open OUnit2

(** TODO: OUnit2 should detect test suites automatically. *)
let all_suites = [
  TestPunycode.suite;
]

let () = run_test_tt_main ("all" >::: all_suites)
