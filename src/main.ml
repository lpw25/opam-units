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

open OpamState.Types
open Format

let print_unit unit =
  print_space ();
  print_string (OpamUnit.Name.to_string (OpamUnit.name unit))

let print_library lib_units lib =
  let units =
    try
      OpamLibrary.Map.find lib lib_units
    with Not_found -> OpamUnit.Set.empty
  in
    print_space ();
    open_vbox 4;
    print_string (OpamLibrary.Name.to_string (OpamLibrary.name lib));
    print_char ':';
    OpamUnit.Set.iter print_unit units;
    close_box ()

let print_package pkg_libs lib_units pkg =
  let libs =
    try
      OpamPackage.Map.find pkg pkg_libs
    with Not_found -> OpamLibrary.Set.empty
  in
    print_space ();
    open_vbox 4;
    print_string (OpamPackage.Name.to_string (OpamPackage.name pkg));
    print_char ':';
    OpamLibrary.Set.iter (print_library lib_units) libs;
    close_box ();
    print_space ()

let print_units t s =
  let pkgs = t.installed in
  let pkg_libs = OpamUnitsState.package_libraries s in
  let lib_units = OpamUnitsState.library_units s in
    open_vbox 0;
    OpamPackage.Set.iter (print_package pkg_libs lib_units) pkgs;
    close_box ()

let print_package_units t s name =
  let pkgs = t.installed in
  let pkg_libs = OpamUnitsState.package_libraries s in
  let lib_units = OpamUnitsState.library_units s in
    try
      let pkg =
        OpamPackage.Set.find (fun pkg -> name = (OpamPackage.name pkg)) pkgs
      in
        print_package pkg_libs lib_units pkg
    with Not_found ->
      OpamGlobals.warning
        "Package %s not installed"
        (OpamPackage.Name.to_string name)

let print_import_graph t s =
  let g = OpamUnitsState.unit_import_graph s in
    OpamUnit.Graph.Dot.output_graph stdout g

let print_package_import_graph t s name =
  let pkgs = t.installed in
    try
      let pkg =
        OpamPackage.Set.find (fun pkg -> name = (OpamPackage.name pkg)) pkgs
      in
      let g = OpamUnitsState.package_unit_import_graph s pkg in
        OpamUnit.Graph.Dot.output_graph stdout g
    with Not_found ->
      OpamGlobals.warning
        "Package %s not installed"
        (OpamPackage.Name.to_string name)

let opam_units name_opt graph =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  let t = OpamState.load_state "opam-units" in
  let s = OpamUnitsState.load_state t in
    match name_opt with
      Some name ->
        let name = OpamPackage.Name.of_string name in
          if graph then print_package_import_graph t s name
          else print_package_units t s name
    | None ->
        if graph then print_import_graph t s
        else print_units t s

open Cmdliner

let package =
  let doc = "Display units from $(docv)" in
  let docv = "PACKAGE" in
    Arg.(value & pos 0 (some string) None & info ~docv ~doc [])

let graph =
  let doc = "Print a graph of units in dot format" in
    Arg.(value & flag & info ~doc ["graph"])

let opam_units =
  let doc = "Remove a package from the Opam digest database" in
  let info = Term.info ~doc "opam-units" in
    (Term.(pure opam_units $package $graph), info)

let () =
  try
    match Term.eval opam_units with
      `Error _ -> exit 1
    | _ -> exit 0
  with OpamGlobals.Exit i -> exit i
