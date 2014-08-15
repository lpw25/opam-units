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

(** Libraries *)

(** Names *)
module Name: sig
  include OpamMisc.ABSTRACT

  (** Compare two library names *)
  val compare: t -> t -> int
end

include OpamMisc.ABSTRACT

(** Create a new library *)
val create: OpamPackage.t -> Name.t -> t

(** Return the library's package *)
val package: t -> OpamPackage.t

(** Return the library's name *)
val name: t -> Name.t

(** Compare two libraries *)
val compare: t -> t -> int

(** Are two libraries equal ? *)
val equal: t -> t -> bool

(** Hash a library *)
val hash: t -> int
