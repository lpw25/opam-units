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

module Digest = struct

  type t = Digest.t

  let create x = x

  let digest x = x

  let to_string = Digest.to_hex

  let of_string = Digest.from_hex

  let compare = Digest.compare

  let to_json x =
    `String (to_string x)

  module O = struct
    type t = Digest.t
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

module Name = struct

  type t = string

  let to_string x = x

  let of_string x =
    let len = String.length x in
    if len = 0 then failwith "Empty unit name";
    begin
      match x.[0] with
      | 'A' .. 'Z' ->
        for i = 1 to (len - 1) do
          match x.[i] with
          | 'A' .. 'Z'
          | 'a' .. 'z'
          | '0' .. '9'
          | '_' | '\'' -> ()
          | c ->
              failwith
                (Printf.sprintf "Invalid character %c in unit name %S" c x)
        done
      | c ->
          failwith
            (Printf.sprintf "Invalid character %c starts unit name %S" c x)
    end;
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
  { library: OpamLibrary.t;
    name: Name.t;
    digest: Digest.t; }

let create library name digest = { library; name; digest }

let library unit = unit.library

let name unit = unit.name

let digest unit = unit.digest

let library_name unit = OpamLibrary.name unit.library

let package unit = OpamLibrary.package unit.library

let package_name unit = OpamPackage.name (OpamLibrary.package unit.library)

let package_version unit = OpamPackage.version (OpamLibrary.package unit.library)

let lib_sep = '/'

let name_sep = ':'

let of_string_opt s =
  if OpamMisc.contains s ' ' || OpamMisc.contains s '\n' then
    None
  else match OpamMisc.cut_at s lib_sep with
  | None -> None
  | Some (lib, s) ->
      match OpamMisc.cut_at s name_sep with
      | None -> None
      | Some (name, digest) ->
                Some { library = OpamLibrary.of_string lib;
                       name = Name.of_string name;
                       digest = Digest.of_string digest }

let of_string s =
  match of_string_opt s with
  | Some x -> x
  | None -> failwith (Printf.sprintf "Invalid unit %s" s)

let to_string unit =
  Printf.sprintf "%s%c%s%c%s"
    (OpamLibrary.to_string unit.library) lib_sep
    (Name.to_string unit.name) name_sep
    (Digest.to_string unit.digest)

let compare unit1 unit2 =
  match OpamLibrary.compare unit1.library unit2.library with
  | 0 -> begin
      match Name.compare unit1.name unit2.name with
      | 0 -> Digest.compare unit1.digest unit2.digest
      | i -> i
    end
  | i -> i

let hash = Hashtbl.hash

let equal unit1 unit2 =
  compare unit1 unit2 = 0

let to_json unit =
  `O [ ("library", OpamLibrary.to_json (library unit));
       ("name", Name.to_json (name unit));
       ("digest", Digest.to_json (digest unit)); ]

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
end

module Set = OpamMisc.Set.Make(O)

module Map = OpamMisc.Map.Make(O)

module Graph = struct
  module Vertex = struct
    include O
    let equal = equal
    let hash = hash
  end
  module PG = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
  module Topological = Graph.Topological.Make(PG)
  module Traverse = Graph.Traverse.Dfs(PG)
  module Components = Graph.Components.Make(PG)
  module Parallel = OpamParallel.Make(struct
    let string_of_vertex = to_string
    include PG
    include Topological
    include Traverse
    include Components
  end)
  module Dot = Graph.Graphviz.Dot (struct
      let edge_attributes _ = []
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes _ = []
      (* Don't print digests in labels, and quote them *)
      let vertex_name unit =
        Printf.sprintf "\"%s%c%s\""
          (OpamLibrary.to_string unit.library) lib_sep
          (Name.to_string unit.name)
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
      include PG
    end)
  include PG
  include Graph.Oper.I(PG)
end
