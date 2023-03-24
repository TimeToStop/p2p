open Types

type 'a maybe_t = Value of 'a | Error of string

let inet_addr_of_string (str : string) : Unix.inet_addr maybe_t =
  try Value (Unix.inet_addr_of_string str) with Failure msg -> Error msg

let port_of (str : string) : int maybe_t =
  let get_int (x : string) : int maybe_t =
    try Value (int_of_string x) with Failure msg -> Error msg
  in
  let result = get_int str in
  match result with
  | Value x when 0 < x && x < 65536 -> result
  | Value _ -> Error "port is out of bounds"
  | Error _ -> result

let get_text cmd host port msg =
  let pos = 3 + String.length cmd + String.length host + String.length port in
  String.sub msg pos (String.length msg - pos)

let parse_host_port (host : string) (port : string) :
    (Unix.inet_addr * int) maybe_t =
  match (inet_addr_of_string host, port_of port) with
  | Error host_err, _ -> Error host_err
  | Value _, Error port_err -> Error port_err
  | Value host_addr, Value port_num -> Value (host_addr, port_num)

let parse_join_request_peers (fd : Lwt_unix.file_descr) (words : string list) :
    peers_request_t =
  let helper (host : string) (port : string) : peers_request_t =
    match parse_host_port host port with
    | Value (addr, port_num) -> Join (fd, addr, port_num)
    | Error msg -> Unknown msg
  in
  match words with
  | [_; host; port] -> helper host port
  | _ -> Unknown "\"join\" requires: ip port"

let parse_close_request_peers (fd : Lwt_unix.file_descr) (words : string list) :
    peers_request_t =
  let helper (host : string) (port : string) : peers_request_t =
    match parse_host_port host port with
    | Value (addr, port_num) -> Close (fd, addr, port_num)
    | Error msg -> Unknown msg
  in
  match words with
  | [_; host; port] -> helper host port
  | _ -> Unknown "\"close\" requires: ip port"

let parse_join_request_discovery (fd : Lwt_unix.file_descr) (words : string list)
    : discovery_request_t =
  let helper (host : string) (port : string) : discovery_request_t =
    match parse_host_port host port with
    | Value (addr, port_num) -> Join (fd, addr, port_num)
    | Error msg -> Unknown msg
  in
  match words with
  | [_; host; port] -> helper host port
  | _ -> Unknown "\"join\" requires: ip port"

let parse_close_request_discovery (fd : Lwt_unix.file_descr)
    (words : string list) : discovery_request_t =
  let helper (host : string) (port : string) : discovery_request_t =
    match parse_host_port host port with
    | Value (addr, port_num) -> Close (fd, addr, port_num)
    | Error msg -> Unknown msg
  in
  match words with
  | [_; host; port] -> helper host port
  | _ -> Unknown "\"close\" requires: ip port"

let parse_msg_request (_ : Lwt_unix.file_descr) (words : string list)
    (request : string) : peers_request_t =
  let helper (host : string) (port : string) : peers_request_t =
    match parse_host_port host port with
    | Value (addr, port_num) ->
        Msg (addr, port_num, get_text "msg" host port request)
    | Error msg -> Unknown msg
  in
  match words with
  | _ :: host :: port :: _ -> helper host port
  | _ -> Unknown "\"msg\" requires: ip port msg"

let timestamp () : string =
  let current_time = Unix.localtime (Unix.time ()) in
  let hour = current_time.tm_hour in
  let minute = current_time.tm_min in
  let second = current_time.tm_sec in
  Printf.sprintf "%02d:%02d:%02d" hour minute second
