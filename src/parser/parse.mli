
(* elpi: embedded lambda prolog interpreter                                  *)
(* license: GNU Lesser General Public License Version 2.1 or later           *)
(* ------------------------------------------------------------------------- *)

open Elpi_util
open Elpi_lexer_config

exception ParseError of Util.Loc.t * string

module type Parser = sig
  val program : file:string -> Ast.Program.t
  val goal : loc:Util.Loc.t -> text:string -> Ast.Goal.t
  
  val goal_from : loc:Util.Loc.t -> Lexing.lexbuf -> Ast.Goal.t
  (** [cwd] overrides the physical base for relative accumulations, otherwise
      the directory of [loc.source_name] is used. *)
  val program_from : ?cwd:string -> loc:Util.Loc.t -> digest:Digest.t -> Lexing.lexbuf -> Ast.Program.t
end

module type Parser_w_Internals = sig
  include Parser

  module Internal : sig
    val infix_SYMB : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> Ast.Func.t
    val prefix_SYMB : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> Ast.Func.t
    val postfix_SYMB : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> Ast.Func.t
  end
end

module type Config = sig
  val versions : (int * int * int) Util.StrMap.t
  val resolver : ?cwd:string -> unit:string -> unit -> string
end

module Make(C : Config) : Parser_w_Internals

(** Parse the physical files returned by [C.resolver], but retain [Source.name]
    in AST file identities and locations. Relative accumulations continue to
    resolve from the physical parent directory. *)
module Make_with_source_names(C : Config)(Source : sig
  val name : string -> string
end) : Parser_w_Internals
