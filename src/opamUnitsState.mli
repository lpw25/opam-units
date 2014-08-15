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

(** Opam units state *)

(** Opam units state *)
type state

(** Load the units state. *)
val load_state : OpamState.state -> state

(** Units *)
val units: state -> OpamUnit.Set.t

(** Map from units to their .cmi files *)
val unit_file: state -> OpamFilename.t OpamUnit.Map.t

(** Graph with edges to units from their imports. Each call produces a
    fresh (mutable) graph.

    It is possible that we cannot decide to which unit an import
    refers. In this case the import is ignored and a warning is
    emitted. *)
val unit_import_graph: state -> OpamUnit.Graph.t

(** Libraries *)
val libraries: state -> OpamLibrary.Set.t

(** Map from libraries to their associated directory *)
val library_directory: state -> OpamFilename.Dir.t OpamLibrary.Map.t

(** Map from libraries to their units *)
val library_units: state -> OpamUnit.Set.t OpamLibrary.Map.t

(** The graph produced by [unit_import_graph] restricted to units within
    a given library *)
val library_unit_import_graph: state -> OpamLibrary.t -> OpamUnit.Graph.t

(** Map from packages to their associated libraries *)
val package_libraries: state -> OpamLibrary.Set.t OpamPackage.Map.t

(** Map from packages to their associated units *)
val package_units: state -> OpamUnit.Set.t OpamPackage.Map.t

(** The graph produced by [unit_import_graph] restricted to units within
    a given package *)
val package_unit_import_graph: state -> OpamPackage.t -> OpamUnit.Graph.t
