(* elpi: embedded lambda prolog interpreter                                  *)
(* license: GNU Lesser General Public License Version 2.1 or later           *)
(* ------------------------------------------------------------------------- *)

open Elpi_parser
open Elpi_util

let write filename contents =
  let output = open_out filename in
  Fun.protect ~finally:(fun () -> close_out output)
    (fun () -> output_string output contents)

let temporary_directory () =
  let path = Filename.temp_file "elpi-source-names" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

let first = temporary_directory ()
let second = temporary_directory ()
let file root name = Filename.concat root name
let rec remove_directory root =
  Array.iter (fun name ->
    let path = file root name in
    if Sys.is_directory path then remove_directory path else Sys.remove path)
    (Sys.readdir root);
  Unix.rmdir root
let () = at_exit (fun () -> List.iter remove_directory [first; second])

let source_name name =
  let parent = Filename.dirname name in
  let root = if parent = first then "first" else if parent = second then "second" else "unmapped" in
  "coq://" ^ root ^ "/" ^ Filename.basename name

module Config = struct
  let versions = Util.StrMap.empty
  let resolver = Util.std_resolver ~paths:[] ()
end
module Parser = Parse.Make_with_source_names(Config)(struct let name = source_name end)
module Default = Parse.Make(Config)
module Literal_build_name = Parse.Make_with_source_names(Config)(struct
  let name path = "coq://literal/_build/context/" ^ Filename.basename path
end)

let children program = match program.Ast.ast with
  | [Ast.Program.Accumulated (_, children)] -> children
  | _ -> assert false

let () =
  List.iter (fun root -> write (file root "child.elpi") "pred p.\np.\n") [first; second];
  write (file first "outer.elpi") "accumulate \"child\".\n";
  let outer = Parser.program ~file:(file first "outer.elpi") in
  assert (outer.file_name = "coq://first/outer.elpi");
  let child = List.hd (children outer) in
  assert (child.file_name = "coq://first/child.elpi");
  (match List.rev child.ast with
  | Ast.Program.Clause { loc; _ } :: _ -> assert (loc.source_name = child.file_name)
  | _ -> assert false);
  let literal = Literal_build_name.program ~file:(file first "child.elpi") in
  assert (literal.file_name = "coq://literal/_build/context/child.elpi");
  (match List.rev literal.ast with
  | Ast.Program.Clause { loc; _ } :: _ -> assert (loc.source_name = literal.file_name)
  | _ -> assert false);
  (* Backward-compatible parsing still keeps the resolver's physical identity. *)
  assert ((Default.program ~file:(file first "outer.elpi")).file_name = file first "outer.elpi");
  (* Legacy clients still resolve beside the cleaned source name under Dune;
     the opt-in named parser resolves beside its physical input instead. *)
  let build = file first "_build" in
  Unix.mkdir build 0o700;
  let context = file build "default" in
  Unix.mkdir context 0o700;
  write (file context "outer.elpi") "accumulate \"child\".\n";
  write (file context "child.elpi") "pred q.\nq.\n";
  let legacy_child = List.hd (children (Default.program ~file:(file context "outer.elpi"))) in
  let named_child = List.hd (children (Parser.program ~file:(file context "outer.elpi"))) in
  assert (legacy_child.digest = Digest.file (file first "child.elpi"));
  assert (named_child.digest = Digest.file (file context "child.elpi"));
  (* Equal contents under distinct identities must not reuse the first AST. *)
  let text = Printf.sprintf "accumulate %S, %S."
    (file first "child") (file second "child") in
  let ast = Parser.program_from ~loc:(Util.Loc.initial "inline")
    ~digest:(Digest.string text) (Lexing.from_string text) in
  assert (List.map (fun ast -> ast.Ast.file_name) (children ast) =
    ["coq://first/child.elpi"; "coq://second/child.elpi"]);
  (* A failed nested parse must restore the physical include directory. *)
  write (file first "bad.elpi") "pred (.";
  write (file first "outer.elpi") "accumulate \"bad\".\n";
  (try ignore (Parser.program ~file:(file first "outer.elpi")); assert false
   with Parse.ParseError _ -> ());
  let text = "accumulate \"child\"." in
  let ast = Parser.program_from ~loc:(Util.Loc.initial (file second "inline.v"))
    ~digest:(Digest.string text) (Lexing.from_string text) in
  assert ((List.hd (children ast)).file_name = "coq://second/child.elpi");
  (* Inline logical locations can retain their own physical include directory. *)
  let ast = Parser.program_from ~cwd:second
    ~loc:(Util.Loc.initial "coq://second/inline.v")
    ~digest:(Digest.string text) (Lexing.from_string text) in
  assert ((List.hd (children ast)).file_name = "coq://second/child.elpi");
  (* Runtime source resolution must not change the serialized identity or key. *)
  let loc = Util.Loc.initial "coq://first/child.elpi" in
  let key = Util.Loc.show loc in
  Util.Loc.set_source_name_resolver (fun _ -> file second "child.elpi");
  assert (Util.Loc.show loc = key);
  assert (loc.source_name = "coq://first/child.elpi");
  assert (Util.Loc.source_name loc = file second "child.elpi");
  let specs = Util.Loc.source_name loc ^ ".specs" in
  write specs "portable source IO\n";
  assert (Sys.file_exists (file second "child.elpi.specs"));
  Sys.remove specs;
  assert (Util.Loc.show_for_display loc <> key);
  Util.Loc.set_source_name_resolver Fun.id;
  assert (Util.Loc.source_name loc = loc.source_name)
