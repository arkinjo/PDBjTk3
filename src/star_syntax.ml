(* data name is preceded by '_' *)
type data_name = Underscore of string

type data_value = 
    Non_quoted_text_string of string
  | Single_quoted_text_string of string
  | Double_quoted_text_string of string
  | Semi_colon_bounded_text_string of string
  | Frame_code of string

(* loop_ *)
type data_loop_item = 
    Data_value of data_value
  | Stop_
type data_loop_values = data_loop_item list
type data_loop_definition = data_loop_field list
and data_loop_field = 
    Data_name of data_name 
(*  | Nested_loop of nested_loop*) (*  no nested loop in mmCIF *) 
(* and nested_loop = data_loop_definition *)

type data = 
    Item of (data_name * data_value)
  | Data_loop of (data_loop_definition * data_loop_values)

(* save frame *)
type save_heading = Save_ of string
type save_frame = save_heading * data list 

(* data block *)
type data_heading = Data_ of string
type data_block_body1 = 
    Data of data 
  | Save_frame of save_frame 
type data_block_body = data_block_body1 list
type data_block =  data_heading * data_block_body

(* global block *)
type global_heading = Global_
type global_block_body = data list
type global_block = global_heading * global_block_body

type star_file1 = 
    Data_block of data_block 
  | Global_block of global_block

type star_file = star_file1 list
