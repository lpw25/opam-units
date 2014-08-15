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

module Name = struct

  type t = string

  let to_string x = x

  let of_string x =
    let len = String.length x in
    if len = 0 then failwith "Empty library name";
    String.iter (function
        | '"'..'~' as c when not (String.contains "!./<=>\\" c) -> ()
        | c ->
          failwith
            (Printf.sprintf "Invalid character %c in library name %S" c x))
      x;
    x

  let compare n1 n2 =
    match compare (String.lowercase n1) (String.lowercase n2) with
    | 0 -> compare n1 n2
    | i -> i

  let to_json x = `String x

  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

type t =
  { package: OpamPackage.t;
    name: Name.t; }

let create package name = { package; name; }

let package lib = lib.package

let name lib = lib.name

let sep = '/'

let of_string_opt s =
  if OpamMisc.contains s ' ' || OpamMisc.contains s '\n' then
    None
  else match OpamMisc.cut_at s sep with
  | None -> None
  | Some (pkg, name) ->
      Some { package = OpamPackage.of_string pkg;
             name = Name.of_string name; }

let of_string s =
  match of_string_opt s with
  | Some x -> x
  | None -> failwith (Printf.sprintf "Invalid library name %s" s)

let to_string lib =
  Printf.sprintf "%s%c%s"
    (OpamPackage.to_string lib.package) sep
    (Name.to_string lib.name)

let compare lib1 lib2 =
  match OpamPackage.compare lib1.package lib2.package with
  | 0 -> Name.compare lib1.name lib2.name
  | i -> i

let hash = Hashtbl.hash

let equal lib1 lib2 =
  compare lib1 lib2 = 0

let to_json lib =
  `O [ ("package", OpamPackage.to_json (package lib));
       ("name", Name.to_json (name lib)); ]

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
end

module Set = OpamMisc.Set.Make(O)

module Map = OpamMisc.Map.Make(O)
