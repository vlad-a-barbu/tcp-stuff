type config =
  { port : int
  ; backlog : int
  }

let default_config = { port = 3000; backlog = 1000 }

let listen_tcp ?(config = default_config) () =
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock @@ ADDR_INET (inet_addr_loopback, config.port);
  listen sock config.backlog;
  Helpers.log_info
  @@ Printf.sprintf "listening on port %d with backlog %d" config.port config.backlog;
  sock
;;

let handler (client, sockaddr) =
  let open Unix in
  let finally () = close client in
  match sockaddr with
  | ADDR_UNIX _ -> failwith "unhandled address type: ADDR_UNIX"
  | ADDR_INET (addr, port) ->
    Helpers.log_info
    @@ Printf.sprintf "handling connection from %s:%d" (string_of_inet_addr addr) port;
    let result =
      match Helpers.recv_tcp client with
      | Ok (n, _bytes) ->
        Helpers.log_info @@ Printf.sprintf "read %d bytes" n;
        Ok ()
      | Error (Unix.Unix_error (Unix.ECONNRESET, _, _)) ->
        Helpers.log_warning ~ctx:"client handler" "connection reset by peer";
        Ok ()
      | Error exn ->
        Helpers.log_exn ~ctx:"client handler" exn;
        Error exn
    in
    finally ();
    result
;;

let rec accept_loop handler sock =
  let handle () =
    try
      match handler @@ Unix.accept sock with
      | Ok _ -> ()
      | Error exn -> Helpers.log_exn ~ctx:"client handler" exn
    with
    | exn -> Helpers.log_exn ~ctx:"accept loop" exn
  in
  handle ();
  accept_loop handler sock
;;

let () =
  let sock = listen_tcp () in
  accept_loop handler sock
;;
