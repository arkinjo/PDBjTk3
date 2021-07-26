%{
open Star_syntax
%}

%token GLOBAL
%token <string> DATA
%token LOOP_
%token STOP_
%token <string> SAVE
%token SAVE_
%token <string> USCORE
%token <string> STRING
%token <string> SQUOTE
%token <string> DQUOTE
%token <string> SEMI
%token <string> FRAME
%token EOF
%start star_file
%type <Star_syntax.star_file> star_file
%%
star_file:
  star_file_list EOF { $1 }
star_file_list:
  data_block { [ Data_block $1 ] }
| global_block { [ Global_block $1 ] }
| data_block star_file_list { (Data_block $1) :: $2 }
| global_block star_file_list { (Global_block $1) :: $2 }
data_block:
  DATA data_block_body { ((Data_ $1),$2) }
;
data_block_body:
  data { [ Data $1 ] }
| SAVE data_list SAVE_ { [ Save_frame (Save_ $1, $2) ] }
| data data_block_body { (Data $1) :: $2 }
| SAVE data_list SAVE_ data_block_body { (Save_frame (Save_ $1, $2)) :: $4 }
;
data_list:
  data { [ $1 ] }
| data data_list { $1 :: $2 }
;
global_block:
  GLOBAL global_block_body { (Global_, $2) }
;
global_block_body:
  data { [$1] }
| data global_block_body { $1 :: $2 }
;
data:
  data_name data_value { Item ($1, $2) }
| data_loop { Data_loop $1 }
;
data_name:
  USCORE { Underscore $1 }
;
data_value:
  STRING { Non_quoted_text_string $1 }
| SQUOTE { Single_quoted_text_string $1 }
| DQUOTE { Double_quoted_text_string $1 }
| SEMI   { Semi_colon_bounded_text_string $1 }
| FRAME  { Frame_code $1 }
;
data_loop:
  LOOP_ data_loop_definition data_loop_values { ($2, $3) }
;
data_loop_definition:
  data_loop_field { [ $1 ] }
| data_loop_field data_loop_definition { $1 :: $2 }
;
data_loop_field:
  data_name { Data_name $1 }
/* | nested_loop { Nested_loop $1 } */
;
/* nested_loop:
  LOOP_ data_loop_definition { $2 }
| LOOP_ data_loop_definition STOP_ { $2 }
; */
data_loop_values:
  data_loop_item { [$1] }
| data_loop_item data_loop_values { $1 :: $2 }
;
data_loop_item:
  data_value { Data_value $1 }
| STOP_  { Stop_ }
;
