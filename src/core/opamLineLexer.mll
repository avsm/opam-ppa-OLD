(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

{ }

rule main words lines = parse
| '\n'              { main [] (List.rev words :: lines) lexbuf }
| [' ' '\t']+       { main words lines lexbuf }
| [^' ' '\t' '\n']+ { main (Lexing.lexeme lexbuf :: words) lines lexbuf }
| _                 { assert false }
| eof               { List.rev (List.rev words :: lines) }

{
  let main = main [] []
}
