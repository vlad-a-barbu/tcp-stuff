open Unix

let connect_local_tcp port timeout_s =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (inet_addr_loopback, port) in
  try
    Unix.setsockopt_float sock SO_RCVTIMEO timeout_s;
    connect sock addr;
    Helpers.log_info @@ Printf.sprintf "connected to localhost:%d" port;
    Ok sock
  with
  | exn ->
    Helpers.log_exn ~ctx:"connect" exn;
    Error exn
;;

let () =
  let port = 3000
  and timeout_s = 5.0
  and n = if Array.length Sys.argv < 2 then 1_000_000 else int_of_string Sys.argv.(1) in
  match connect_local_tcp port timeout_s with
  | Ok sock ->
    (match Helpers.send_tcp sock @@ Bytes.create n with
     | Ok _ ->
       Helpers.log_info @@ Printf.sprintf "sent %d bytes to localhost:%d" n port;
       close sock
     | Error exn ->
       Helpers.log_exn ~ctx:"send_tcp" exn;
       close sock)
  | Error exn -> Helpers.log_exn ~ctx:"connect" exn
;;
