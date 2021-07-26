{
open Star_parser
open Lexing

let last_pos = ref 0
and current_line = ref 0
and current_line_start = ref 0

let init lexbuf =
  current_line := 1;
  current_line_start := lexeme_start lexbuf;
  last_pos := !current_line_start

let pos lexbuf =
  !current_line, !current_line_start, !last_pos, lexeme_start lexbuf

let newline lexbuf =
  incr current_line;
  last_pos := lexeme_end lexbuf;
  current_line_start := !last_pos

let print lexbuf =
  Printf.fprintf stderr "lexbuf:%s:\n" (Lexing.lexeme lexbuf); 
  flush stderr

}

let newline = ['\n']
let break = ['\r']
let space = [' ' '\t' ]
let nonblank = [^ ' ' '\t' '\r' '\n']
rule token = parse
  newline { newline lexbuf; 
	    token lexbuf }
| (space | break) + { last_pos := lexeme_end lexbuf; 
		      token lexbuf }
| '#' [^ '\n']* { last_pos := lexeme_end lexbuf; 
		  token lexbuf }
| "\n;" { newline lexbuf; last_pos := lexeme_start lexbuf; semi ";" lexbuf }
| "__DEBUG__" { last_pos := lexeme_start lexbuf; EOF }
| "global_" { last_pos := lexeme_start lexbuf; GLOBAL }
| "data_" (nonblank+ as s) { last_pos := lexeme_start lexbuf; DATA s }
| "save_" (nonblank+ as s) { last_pos := lexeme_start lexbuf; SAVE s }
| "save_" { last_pos := lexeme_start lexbuf; SAVE_ }
| "loop_" { last_pos := lexeme_start lexbuf; LOOP_ }
| "stop_" { last_pos := lexeme_start lexbuf; STOP_ }
| '_' (nonblank+ as s) { last_pos := lexeme_start lexbuf; USCORE s }
| '\'' ('\'' nonblank | [^'\''])* '\'' as s 
    { last_pos := lexeme_start lexbuf; SQUOTE s }
| '\"' ('\"' nonblank | [^ '\"'])* '\"' as s 
    { last_pos := lexeme_start lexbuf; DQUOTE s}
| '$' (nonblank+ as s) { last_pos := lexeme_start lexbuf; FRAME s }
| nonblank+ as s { last_pos := lexeme_start lexbuf; STRING s }
| eof { EOF }
and semi s = parse
  "\n;" { newline lexbuf; SEMI (s ^ "\n;") }
| [^'\n']* as t { semi (s ^ t) lexbuf }
| newline { newline lexbuf; semi (s ^ "\n") lexbuf }
