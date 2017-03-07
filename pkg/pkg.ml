#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "punycode" @@ fun _c ->
  Ok [
    Pkg.mllib "src/punycode.mllib";
    Pkg.test "tests/tests"
  ]
