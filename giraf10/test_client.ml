open Printf
module Sc = Server_comm.Protocol

let _ =
  let host = Sys.argv.(1) in
  let port = int_of_string Sys.argv.(2) in
  let inet_addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET(inet_addr,port) in
  let ic,oc = Unix.open_connection sock_addr in

  Sys.set_signal Sys.sigterm
    (Sys.Signal_handle (fun _ -> 
      prerr_endline "Being killed!"; flush stderr;
      Unix.shutdown_connection ic;
      exit 1));

  output_value oc (Sc.Hello "hisai"); flush oc;
  print_endline (input_line ic); flush stdout;
  Unix.sleep 5;
  Unix.shutdown_connection ic;
  ()

  
