let sock_server = Unix.ADDR_INET(Unix.inet_addr_any,7788)
let sock_server = Unix.ADDR_UNIX("/tmp/hogehgoe.server")

let _ =
  Unix.establish_server
    (fun inchan outchan ->
      print_endline (input_line inchan);
      close_in inchan)
    sock_server

