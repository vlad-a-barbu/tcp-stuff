let max_buf_len = 4096 (* 4KB *)

let recv_tcp sock =
  let buf = Bytes.create max_buf_len in
  let rec go acc =
    let n = Unix.recv sock buf 0 max_buf_len [] in
    if n = 0 then acc else go (Bytes.sub buf 0 n :: acc)
  in
  try
    let bytes = Bytes.concat Bytes.empty @@ List.rev @@ go [] in
    Ok (Bytes.length bytes, bytes)
  with
  | exn -> Error exn
;;

let send_tcp sock bytes =
  let total = Bytes.length bytes in
  let rec go nread =
    let nwrite = min max_buf_len (total - nread) in
    let n = Unix.send sock bytes nread nwrite [] in
    let nread = nread + n in
    if nread < total then go nread else ()
  in
  try
    go 0;
    Ok ()
  with
  | exn -> Error exn
;;

let log_exn ?(oc = stdout) ?(ctx = "global") exn =
  Printf.fprintf oc "[%s] error: %s\n%!" ctx (Printexc.to_string exn)
;;

let log_warning ?(oc = stdout) ?(ctx = "global") msg =
  Printf.fprintf oc "[%s] warning: %s\n%!" ctx msg
;;

let log_info ?(oc = stdout) ?(ctx = "global") msg =
  Printf.fprintf oc "[%s] info: %s\n%!" ctx msg
;;

let log_debug ?(oc = stdout) ?(ctx = "global") msg =
  Printf.fprintf oc "[%s] debug: %s\n%!" ctx msg
;;
