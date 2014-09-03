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

type state =
  { timestamp: float;
    package_timestamp: float OpamPackage.Map.t;
    units: OpamUnit.Set.t;
    unit_file: OpamFilename.t OpamUnit.Map.t;
    unit_imports:
      (OpamUnit.Name.t * OpamUnit.Digest.t) list OpamUnit.Map.t;
    library_directory: OpamFilename.Dir.t OpamLibrary.Map.t; }

let read_state file =
  if not (OpamFilename.exists file) then None
  else
    let ic = OpamFilename.open_in file in
    set_binary_mode_in ic true;
    try
      let magic_no = OpamUnitsConfig.cache_magic_number in
      let magic_len = String.length magic_no in
      let buffer = Bytes.create magic_len in
      really_input ic buffer 0 magic_len;
      if buffer <> magic_no then begin
        close_in ic;
        let pre_magic_len = magic_len - 3 in
        let pre_magic_no = String.sub magic_no 0 pre_magic_len in
        let pre_buffer = String.sub buffer 0 pre_magic_len in
        let msg =
          if pre_buffer <> pre_magic_no then "is not a cache file."
          else if buffer < magic_no then
            "is not a cache file for this version of opam-units.@.\
             It seems to be for an older version of opam-units."
          else
            "is not a cache file for this version of opam-units.@.\
             It seems to be for an newer version of opam-units."
        in
          OpamGlobals.warning "%s %s" (OpamFilename.prettify file) msg;
          None
      end else begin
        let s : state = input_value ic in
        close_in ic;
        Some s
      end
    with End_of_file | Failure _ ->
      close_in ic;
      OpamGlobals.warning
        "Cache file %s is corrupted."
        (OpamFilename.prettify file);
      None

let write_state file s =
  let magic_no = OpamUnitsConfig.cache_magic_number in
  let dir = OpamFilename.dirname file in
  if not (OpamFilename.exists_dir dir) then OpamFilename.mkdir dir;
  let oc = OpamFilename.open_out file in
    set_binary_mode_out oc true;
    try
      output_string oc magic_no;
      output_value oc s;
      close_out oc
    with exn ->
      close_out oc;
      raise exn

let convert_name name =
  OpamUnit.Name.of_string name

let convert_digest digest =
  OpamUnit.Digest.create digest

let convert_imports l =
  List.fold_right
    (fun hd tl ->
       match hd with
       | name, Some digest ->
           (convert_name name, convert_digest digest) :: tl
       | _, None -> tl)
    l []

let warn_cmi_error file = function
  | Cmi_format.Not_an_interface _ ->
      OpamGlobals.warning
        "%s is not a compiled interface."
        (OpamFilename.prettify file)
  | Cmi_format.Wrong_version_interface(_, msg) ->
      OpamGlobals.warning
        "%s is not a compiled interface for this version of OCaml.@.\
         It seems to be for %s version of OCaml."
        (OpamFilename.prettify file) msg
  | Cmi_format.Corrupted_interface _ ->
      OpamGlobals.warning
        "Compiled interface %s is corrupted."
        (OpamFilename.prettify file)

(* Add the compilation unit in a given cmi file *)
let add_cmi lib s file =
  try
    let cmi_info = Cmi_format.read_cmi (OpamFilename.to_string file) in
      match cmi_info.Cmi_format.cmi_crcs with
      | (name, Some digest) :: imports when
              name = cmi_info.Cmi_format.cmi_name ->
          let name = convert_name name in
          let digest = convert_digest digest in
          let imports = convert_imports imports in
          let unit = OpamUnit.create lib name digest in
            { s with units = OpamUnit.Set.add unit s.units;
                     unit_file = OpamUnit.Map.add unit file s.unit_file;
                     unit_imports =
                       OpamUnit.Map.add unit imports s.unit_imports; }
      | _ ->
          OpamGlobals.warning
            "Compiled interface %s is corrupted."
            (OpamFilename.prettify file);
          s
  with Cmi_format.Error err ->
    warn_cmi_error file err;
    s

let findlib_units_state = FindlibUnits.state ()

(* Add the compilation units in a given library. *)
let add_library s lib =
  let name = OpamLibrary.Name.to_string (OpamLibrary.name lib) in
  try
    let units = FindlibUnits.of_package findlib_units_state name in
    let s = FindlibUnits.Map.fold (fun _lib unit_map s ->
      FindlibUnits.Map.fold (fun _unit_name { FindlibUnits.cmi } s ->
        add_cmi lib s (OpamFilename.of_string cmi)
      ) unit_map s
    ) units s in
    let dir = OpamFilename.Dir.of_string (Findlib.package_directory name) in
    { s with library_directory =
        OpamLibrary.Map.add lib dir s.library_directory }
  with Findlib.No_such_package _ ->
    OpamGlobals.warning "Library %s is not installed." name;
    s

let install_time t pkg =
  let file = OpamPath.Switch.install t.root t.switch (OpamPackage.name pkg) in
  let stats = Unix.stat (OpamFilename.to_string file) in
    stats.Unix.st_mtime

(* Add the compilation units in a given package. *)
let add_package t pkg s =
  let lib_names =
    match OpamState.repository_and_prefix_of_package t pkg with
    | None -> []
    | Some (repo, prefix) ->
        let dir = OpamPath.Repository.packages repo prefix pkg in
        let file = OpamFilename.OP.(dir // "findlib") in
        let lines = OpamFile.Lines.safe_read file in
          List.flatten lines
  in
  let libs =
    List.map
      (fun name ->
         let name = OpamLibrary.Name.of_string name in
           OpamLibrary.create pkg name)
      lib_names
  in
  let timestamp = install_time t pkg in
  let s =
      { s with timestamp = timestamp;
               package_timestamp =
                 OpamPackage.Map.add pkg timestamp s.package_timestamp; }
  in
    List.fold_left add_library s libs

let remove_package t pkg s =
  let check_pkg pkg' = OpamPackage.equal pkg pkg' in
  let check_lib lib = check_pkg (OpamLibrary.package lib) in
  let check_unit unit = check_lib (OpamUnit.library unit) in
  let check_binding check x y = check x in
  { timestamp = s.timestamp;
    package_timestamp =
      OpamPackage.Map.filter (check_binding check_pkg) s.package_timestamp;
    units = OpamUnit.Set.filter check_unit s.units;
    unit_file = OpamUnit.Map.filter (check_binding check_unit) s.unit_file;
    unit_imports =
      OpamUnit.Map.filter (check_binding check_unit) s.unit_imports;
    library_directory =
      OpamLibrary.Map.filter (check_binding check_lib) s.library_directory; }

let init_state t file =
  OpamGlobals.msg
    "Creating a cache of units metadata in %s ...\n"
    (OpamFilename.prettify file);
  let s =
    { timestamp = 0.0;
      package_timestamp = OpamPackage.Map.empty;
      units = OpamUnit.Set.empty;
      unit_file = OpamUnit.Map.empty;
      unit_imports = OpamUnit.Map.empty;
      library_directory = OpamLibrary.Map.empty; }
  in
  let s = OpamPackage.Set.fold (add_package t) t.installed s in
    write_state file s;
    s

let update_state t s file =
  OpamGlobals.msg
    "Updating the cache of units metadata in %s ...\n"
    (OpamFilename.prettify file);
  let remove =
    OpamPackage.Set.of_list (OpamPackage.Map.keys s.package_timestamp)
  in
  let add = t.installed in
  let remove, add =
    OpamPackage.Set.fold
      (fun pkg ((remove, add) as acc) ->
         if OpamPackage.Set.mem pkg add then
           let told = OpamPackage.Map.find pkg s.package_timestamp in
           let tnew = install_time t pkg in
             if tnew > told then acc
             else
               let remove = OpamPackage.Set.remove pkg remove in
               let add = OpamPackage.Set.remove pkg add in
                 (remove, add)
         else acc)
      remove (remove, add)
  in
  let s = OpamPackage.Set.fold (remove_package t) remove s in
  let s = OpamPackage.Set.fold (add_package t) add s in
    write_state file s;
    s

let last_install_time t =
  let file = OpamPath.Switch.installed t.root t.switch in
  let stats = Unix.stat (OpamFilename.to_string file) in
    stats.Unix.st_mtime

let load_state t =
  let share = OpamPath.Switch.share t.root t.switch OpamUnitsConfig.package_name in
  let file = OpamFilename.create share OpamUnitsConfig.cache_file in
  match read_state file with
    Some s ->
      let told = s.timestamp in
      let tnew = last_install_time t in
        if told >= tnew then s
        else update_state t s file
  | None -> init_state t file

let units s = s.units

let unit_file s = s.unit_file

let disambiguate_by_package unit l =
  let pkg = OpamUnit.package unit in
    List.filter
      (fun import -> OpamPackage.equal pkg (OpamUnit.package import))
      l

let disambiguate_by_library unit l =
  let lib = OpamUnit.library unit in
    List.filter
      (fun import -> OpamLibrary.equal lib (OpamUnit.library import))
      l

let disambiguate_by_package_dependencies unit l =
  let choices = List.map OpamUnit.to_string l in
  let s =
    Format.sprintf
      "TODO: Disambiguate %s by package dependencies of %s"
      (String.concat " " choices)
      (OpamUnit.to_string unit)
  in
    failwith s

let disambiguate_by_library_dependencies unit l =
  let choices = List.map OpamUnit.to_string l in
  let s =
    Format.sprintf
      "TODO: Disambiguate %s by library dependencies of %s"
      (String.concat " " choices)
      (OpamUnit.to_string unit)
  in
    failwith s

let lookup_import units unit (name, digest) =
  let units =
    OpamUnit.Set.filter
      (fun unit ->
         let name' = OpamUnit.name unit in
         let digest' = OpamUnit.digest unit in
           (name = name')
           && (digest = digest'))
      units
  in
    match OpamUnit.Set.elements units with
    | [] -> None
    | [x] -> Some x
    | l ->
        match disambiguate_by_package unit l with
          | [] -> begin
              match disambiguate_by_package_dependencies unit l with
              | [] ->
                  failwith "TODO: Emit a proper warning for failed disambiguation"
              | [x] -> Some x
              | l -> begin
                  match disambiguate_by_library_dependencies unit l with
                  | [] ->
                      failwith "TODO: Emit a proper warning for failed disambiguation"
                  | [x] -> Some x
                  | l ->
                      failwith "TODO: Emit a proper warning for failed disambiguation"
              end
          end
          | [x] -> Some x
          | l ->
              match disambiguate_by_library unit l with
                | [] -> begin
                    match disambiguate_by_library_dependencies unit l with
                    | [] ->
                        failwith "TODO: Emit a proper warning for failed disambiguation"
                    | [x] -> Some x
                    | l ->
                        failwith "TODO: Emit a proper warning for failed disambiguation"
                end
                | [x] -> Some x
                | l ->
                    failwith "TODO: Emit a proper warning for failed disambiguation"

let unit_import_graph s =
  let g = OpamUnit.Graph.create () in
  let add_import unit import =
    match lookup_import s.units unit import with
      Some unit' -> OpamUnit.Graph.add_edge g unit' unit;
    | None -> ()
  in
  let add_unit unit =
    OpamUnit.Graph.add_vertex g unit;
    let imports = OpamUnit.Map.find unit s.unit_imports in
      List.iter (add_import unit) imports
  in
    OpamUnit.Set.iter add_unit s.units;
    g

let libraries s =
  OpamUnit.Set.fold
    (fun unit libs ->
       OpamLibrary.Set.add (OpamUnit.library unit) libs)
    s.units OpamLibrary.Set.empty

let library_directory s = s.library_directory

let library_units s =
  OpamUnit.Set.fold
    (fun unit map ->
       let lib = OpamUnit.library unit in
       let lib_units =
         try
           OpamUnit.Set.add unit (OpamLibrary.Map.find lib map)
         with Not_found -> OpamUnit.Set.singleton unit
       in
         OpamLibrary.Map.add lib lib_units map)
    s.units OpamLibrary.Map.empty

let library_unit_import_graph s lib =
  let g = OpamUnit.Graph.create () in
  let units =
    OpamUnit.Set.filter (fun unit -> OpamLibrary.equal lib (OpamUnit.library unit)) s.units
  in
  let add_import unit import =
    match lookup_import units unit import with
      Some unit' -> OpamUnit.Graph.add_edge g unit' unit;
    | None -> ()
  in
  let add_unit unit =
    OpamUnit.Graph.add_vertex g unit;
    let imports = OpamUnit.Map.find unit s.unit_imports in
      List.iter (add_import unit) imports
  in
    OpamUnit.Set.iter add_unit units;
    g

let package_libraries s =
  OpamLibrary.Set.fold
    (fun lib map ->
       let pkg = OpamLibrary.package lib in
       let pkg_libs =
         try
           OpamLibrary.Set.add lib (OpamPackage.Map.find pkg map)
         with Not_found -> OpamLibrary.Set.singleton lib
       in
         OpamPackage.Map.add pkg pkg_libs map)
    (libraries s) OpamPackage.Map.empty

let package_units s =
  OpamUnit.Set.fold
    (fun unit map ->
       let pkg = OpamUnit.package unit in
       let pkg_units =
         try
           OpamUnit.Set.add unit (OpamPackage.Map.find pkg map)
         with Not_found -> OpamUnit.Set.singleton unit
       in
         OpamPackage.Map.add pkg pkg_units map)
    s.units OpamPackage.Map.empty

let package_unit_import_graph s pkg =
  let g = OpamUnit.Graph.create () in
  let units =
    OpamUnit.Set.filter (fun unit -> OpamPackage.equal pkg (OpamUnit.package unit)) s.units
  in
  let add_import unit import =
    match lookup_import units unit import with
      Some unit' -> OpamUnit.Graph.add_edge g unit' unit;
    | None -> ()
  in
  let add_unit unit =
    OpamUnit.Graph.add_vertex g unit;
    let imports = OpamUnit.Map.find unit s.unit_imports in
      List.iter (add_import unit) imports
  in
    OpamUnit.Set.iter add_unit units;
    g
