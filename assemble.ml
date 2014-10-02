(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Assemblage

(* OCamlfind packages *)
let lib_pkgs = [
  pkg "opam-lib";
  pkg "opam-lib.client";
  pkg "compiler-libs.common";
  pkg "ocamlfind-units";
  pkg "ocamlgraph";
]
let bin_pkgs = [pkg "cmdliner"]

(* Compilation units *)
let opamUnitsConfig  = unit "opamUnitsConfig" (`Path ["src"])
let opamLibrary = unit "opamLibrary" (`Path ["src"])
let opamUnit = unit "opamUnit" (`Path ["src"])
let opamUnitsState = unit "opamUnitsState" (`Path ["src"])
let main = unit "main" (`Path ["src"])

(* Binary and library *)
let l = lib ~deps:lib_pkgs "opam-units" (`Units [opamLibrary; opamUnit; opamUnitsConfig; opamUnitsState])
let b = bin ~deps:(l :: bin_pkgs) "opam-units" (`Units [main])

let () = assemble (project "opam-units" [b;l])
