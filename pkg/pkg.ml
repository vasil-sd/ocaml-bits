#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "bits" @@ fun c ->
  Ok [ Pkg.mllib "src/bits.mllib";
       Pkg.clib "src/libbits.clib";
       (*Pkg.test "test/test";*) ]
