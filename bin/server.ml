open Lwt
open Types
open Utils

let listen_address : Unix.inet_addr = Unix.inet_addr_loopback

let port : int = 9002

let backlog : int = 10

let peers : peer_t list ref = ref []

let add_to_peers (peer : peer_t) : unit =
  peers :=
    List.filter (fun x -> not (is_same_origin x peer)) !peers
    |> List.append [peer]

let remove_from_peers (peer : peer_t) : unit =
  peers := List.filter (fun x -> not (is_same_origin x peer)) !peers

let parse_request (fd : Lwt_unix.file_descr) (request : string) :
    discovery_request_t =
  let words =
    Str.split (Str.regexp "[ \t\n\r\x0c]+") request
    |> List.filter (fun x -> x <> "")
  in
  match words with
  | "join" :: _ -> parse_join_request_discovery fd words
  | "close" :: _ -> parse_close_request_discovery fd words
  | _ -> Unknown "unknown command or empty"

let execute_request : discovery_request_t -> discovery_response_t = function
  | Join peer ->
      add_to_peers peer ;
      OnJoin (List.filter (fun x -> not (is_same_origin x peer)) !peers)
  | Close peer -> remove_from_peers peer ; OnClose
  | Unknown error -> OnUnknown error

let to_string : discovery_response_t -> string = function
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
