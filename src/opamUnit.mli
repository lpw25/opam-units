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

(** Compilation units *)

(** Digests *)
module Digest: sig

  include OpamMisc.ABSTRACT

  (* Create a digest *)
  val create: Digest.t -> t

  (** Return the digest value *)
  val digest: t -> Digest.t

  (** Compare two digests *)
  val compare: t -> t -> int
end

(** Names *)
module Name: sig
  include OpamMisc.ABSTRACT

  (** Compare two unit names *)
  val compare: t -> t -> int
end

include OpamMisc.ABSTRACT

(** Create a new unit *)
val create: OpamLibrary.t -> Name.t -> Digest.t -> t

(** Return the unit's library *)
val library: t -> OpamLibrary.t

(** Return the unit's name *)
val name: t -> Name.t

(** Return the unit's digest *)
val digest: t -> Digest.t

(** Return the unit's library name *)
val library_name: t -> OpamLibrary.Name.t

(** Return the unit's package *)
val package: t -> OpamPackage.t

(** Return the unit's package name *)
val package_name: t -> OpamPackage.Name.t

(** Return the unit's package version *)
val package_version: t -> OpamPackage.Version.t

(** Compare two units *)
val compare: t -> t -> int

(** Are two units equal ? *)
val equal: t -> t -> bool

(** Hash a unit *)
val hash: t -> int

(** Graphs *)
module Graph: OpamParallel.GRAPH with type V.t = t
