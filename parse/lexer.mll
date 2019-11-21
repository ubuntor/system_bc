{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
| whitespace {read lexbuf}
| newline {next_line lexbuf; read lexbuf}
| "fun" { FUN }
| 'z' { Z }
| "s0" { S0 }
| "s1" { S1 }
| "case" { CASE }
| "rec" { REC }
| "with" { WITH }
| "->" { ARROW }
| '{' { LEFT_BRACE }
| '}' { RIGHT_BRACE }
| ':' { COLON }
| '(' { LEFT_PAREN }
| ')' { RIGHT_PAREN }
| '|' { BAR }
| "[]" { BOX }
| "nat" { NAT }
| id { ID (Lexing.lexeme lexbuf) }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
| eof { EOF }
