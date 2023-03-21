open Lwt

type peer_t = Lwt_unix.file_descr * Unix.inet_addr * int

type request_t = Join of peer_t | Close of peer_t | Unknown of string

type response_t = OnJoin of peer_t list | OnClose | OnUnknown of string

type 'a maybe_t = Value of 'a | Error of string

let listen_address = Unix.inet_addr_loopback

let port = 9002

let backlog = 10

let peers = ref []

let inet_addr_of_string (str : string) : Unix.inet_addr maybe_t =
  try Value (Unix.inet_addr_of_string str) with Failure msg -> Error msg

let port_of (str : string) : int maybe_t =
  let get_int (x : string) =
    try Value (int_of_string x) with Failure msg -> Error msg
  in
  let result = get_int str in
  match result with
  | Value x when 0 < x && x < 65536 -> result
  | Value _ -> Error "port is out of bounds"
  | Error _ -> result

let timestamp () =
  let current_time = Unix.localtime (Unix.time ()) in
  let hour = current_time.tm_hour in
  let minute = current_time.tm_min in
  let second = current_time.tm_sec in
  Printf.sprintf "%02d:%02d:%02d" hour minute second

let is_same_origin (a : peer_t) (b : peer_t) : bool =
  let _, addr_a, port_a = a in
  let _, addr_b, port_b = b in
  addr_a = addr_b && port_a = port_b

let peer_to_string : peer_t -> string = function
  | _, addr, port -> Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let add_to_peers (peer : peer_t) : unit =
  peers :=
    List.filter (fun x -> not (is_same_origin x peer)) !peers
    |> List.append [peer]

let remove_from_peers (peer : peer_t) : unit =
  peers := List.filter (fun x -> not (is_same_origin x peer)) !peers

let rec peers_as_string : peer_t list -> string = function
  | [] -> "\n"
  | peer :: rest -> peer_to_string peer ^ " " ^ peers_as_string rest

let parse_host_port (host : string) (port : string) :
    (Unix.inet_addr * int) maybe_t =
  match (inet_addr_of_string host, port_of port) with
  | Error host_err, _ -> Error host_err
  | Value _, Error port_err -> Error port_err
  | Value host_addr, Value port_num -> Value (host_addr, port_num)

let parse_join_request (fd : Lwt_unix.file_descr) (words : string list) :
    request_t =
  let helper (host : string) (port : string) : request_t =
    match parse_host_port host port with
    | Value (addr, port_num) -> Join (fd, addr, port_num)
    | Error msg -> Unknown msg
  in
  match words with
  | [_; host; port] -> helper host port
  | _ -> Unknown "\"join\" requires: ip port"

let parse_close_request (fd : Lwt_unix.file_descr) (words : string list) :
    request_t =
  let helper (host : string) (port : string) : request_t =
    match parse_host_port host port with
    | Value (addr, port_num) -> Close (fd, addr, port_num)
    | Error msg -> Unknown msg
  in
  match words with
  | [_; host; port] -> helper host port
  | _ -> Unknown "\"close\" requires: ip port"

let parse_request (fd : Lwt_unix.file_descr) (request : string) : request_t =
  let words =
    Str.split (Str.regexp "[ \t\n\r\x0c]+") request
    |> List.filter (fun x -> x <> "")
  in
  match words with
  | "join" :: _ -> parse_join_request fd words
  | "close" :: _ -> parse_close_request fd words
  | _ -> Unknown "unknown command or empty"

let execute_request : request_t -> response_t = function
  | Join peer ->
      add_to_peers peer ;
      OnJoin (List.filter (fun x -> not (is_same_origin x peer)) !peers)
  | Close peer -> remove_from_peers peer ; OnClose
  | Unknown error -> OnUnknown error

let to_string : response_t -> string = function
  | OnJoin peers -> peers_as_string peers
  | OnClose -> ""
  | OnUnknown error -> error

let handle_request (fd : Lwt_unix.file_descr) (request : string) : string =
  let request_command = parse_request fd request in
  let response = execute_request request_command in
  Logs.info (fun m ->
      m "%s\n\trequest:\n\t\t%s\n\tresponse:\n\t\t%s" (timestamp ()) request
        (to_string response) ) ;
  to_string response

let rec handle_connection (fd : Lwt_unix.file_descr) (ic : Lwt_io.input_channel)
    (oc : Lwt_io.output_channel) () : unit Lwt.t =
  Lwt_io.read_line_opt ic
  >>= function
  | Some request ->
      let response = handle_request fd request in
      Lwt_io.write_line oc response
      >>= fun () -> Lwt_io.flush oc >>= handle_connection fd ic oc
  | None -> Lwt.return_unit

let accept_connection (conn : Lwt_unix.file_descr * Lwt_unix.sockaddr) :
    unit Lwt.t =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt.on_failure (handle_connection fd ic oc ()) (fun e ->
      Logs.err (fun m -> m "%s %s" (timestamp ()) (Printexc.to_string e)) ) ;
  Lwt.return_unit

let create_server_socket () : Lwt_unix.file_descr =
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Logs.info (fun m ->
      m "%s Starting on host = %s ; port = %d" (timestamp ())
        (Unix.string_of_inet_addr listen_address)
        port ) ;
  Lwt_main.run (Lwt_unix.bind sock (ADDR_INET (listen_address, port))) ;
  Lwt_unix.listen sock backlog ;
  sock

let create_server (sock : Lwt_unix.file_descr) : unit -> unit Lwt.t =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

let () =
  Logs.set_reporter (Logs.format_reporter ()) ;
  Logs.set_level (Some Logs.Info) ;
  let sock = create_server_socket () in
  let serve = create_server sock in
  Lwt_main.run (serve ())
