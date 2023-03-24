open Lwt
open Types
open Utils

let listen_address = Unix.inet_addr_loopback

let backlog_size = 10

let peers = ref []

let history = ref []

let add_message_to_history (msg : message_t) (history : message_t list) :
    message_t list =
  msg :: history

let message_to_string : message_t -> string = function
  | addr, port, msg ->
      Printf.sprintf "[%s:%s]: %s"
        (Unix.string_of_inet_addr addr)
        (string_of_int port) msg

let rec history_as_string : message_t list -> string = function
  | [] -> ""
  | msg :: rest -> history_as_string rest ^ ";" ^ message_to_string msg

let add_to_peers (peer : peer_t) : unit =
  peers :=
    List.filter (fun x -> not (is_same_origin x peer)) !peers
    |> List.append [peer]

let remove_from_peers (peer : peer_t) : unit =
  peers := List.filter (fun x -> not (is_same_origin x peer)) !peers

let send_data_to_peer (fd : Lwt_unix.file_descr) (data : string) =
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt_io.write_line oc data >>= fun () -> Lwt_io.flush oc

let recieve_response_from_peer (fd : Lwt_unix.file_descr) =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  Lwt_io.read_line_opt ic
  >>= function
  | None -> failwith "expected response" | Some response -> Lwt.return response

let connect_to_peer_on_join peer =
  let _, addr, iport = peer in
  let sockaddr = Unix.ADDR_INET (addr, iport) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr
  >>= fun () ->
  add_to_peers (socket, addr, iport) ;
  Logs_lwt.info (fun m ->
      m "%s Connected to %s:%d" (timestamp ())
        (Unix.string_of_inet_addr addr)
        iport )

let connect_to_rest_peer_impl hostname port =
  let addr = Unix.inet_addr_of_string hostname in
  let iport = int_of_string port in
  let sockaddr = Unix.ADDR_INET (addr, iport) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr
  >>= fun () ->
  Lwt.return (add_to_peers (socket, addr, iport))
  >>= fun _ ->
  send_data_to_peer socket ("join 127.0.0.1 " ^ Sys.argv.(1))
  >>= fun () ->
  recieve_response_from_peer socket
  >>= fun _ ->
  Logs_lwt.info (fun m -> m "%s connect to %s:%d" (timestamp ()) hostname iport)

let connect_to_rest_peer peer =
  match Str.split (Str.regexp "[:]+") peer with
  | [hostname; port] -> connect_to_rest_peer_impl hostname port
  | _ -> failwith ("bad peer msg: " ^ peer)

let connect_to_peer host port =
  let addr = Unix.inet_addr_of_string host in
  let iport = int_of_string port in
  let sockaddr = Unix.ADDR_INET (addr, iport) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () -> Lwt.return (socket, host, port)

let handle_join_response response =
  let peers = Str.split (Str.regexp "[ \t\n\r\x0c]+") response in
  Lwt_list.iter_s connect_to_rest_peer peers

let handle_history_response response =
  let msgs =
    Str.split (Str.regexp "[;]+") response |> List.filter (fun x -> x != "")
  in
  let rec print_msg = function
    | [] -> Lwt.return_unit
    | head :: tail ->
        Logs_lwt.info (fun m -> m "%s %s" (timestamp ()) head)
        >>= fun () -> print_msg tail
  in
  print_msg msgs

let connect_via_peer host port =
  connect_to_peer host port
  >>= fun (socket, host, port) ->
  add_to_peers (socket, Unix.inet_addr_of_string host, int_of_string port) ;
  send_data_to_peer socket ("join 127.0.0.1 " ^ Sys.argv.(1))
  >>= fun () ->
  recieve_response_from_peer socket
  >>= handle_join_response
  >>= fun () ->
  send_data_to_peer socket "history"
  >>= fun () -> recieve_response_from_peer socket >>= handle_history_response

let fetch_history () =
  let sock, _, _ = List.nth !peers 0 in
  send_data_to_peer sock "history"
  >>= fun () ->
  recieve_response_from_peer sock
  >>= fun response ->
  Lwt.return
    (Str.split (Str.regexp "[;]+") response |> List.filter (fun x -> x != ""))

let print_messages msgs =
  let rec print_msg = function
    | [] -> Lwt.return_unit
    | head :: tail ->
        Logs_lwt.info (fun m -> m "%s %s" (timestamp ()) head)
        >>= fun () -> print_msg tail
  in
  print_msg msgs

let connect_via_discovery host port =
  connect_to_peer host port
  >>= fun (socket, host, port) ->
  add_to_peers (socket, Unix.inet_addr_of_string host, int_of_string port) ;
  send_data_to_peer socket ("join 127.0.0.1 " ^ Sys.argv.(1))
  >>= fun () ->
  recieve_response_from_peer socket
  >>= handle_join_response
  >>= fun () ->
  if List.length !peers < 2 then Lwt.return_unit
  else fetch_history () >>= print_messages

let init = function
  | [_; _; "c"; host; port] -> connect_via_peer host port
  | [_; _; "d"; host; port] -> connect_via_discovery host port
  | _ ->
      Logs_lwt.info (fun m ->
          m
            "%s usage:\n\
             <port> c <hostname> <port> - connect via existing peer\n\
             <port> d <hostname> <port> - connect via discovery server"
            (timestamp ()) )

let peers_request_to_string (req : peers_request_t) : string =
  match req with
  | Join p -> "join " ^ peer_to_string p
  | History -> "history"
  | Msg m -> "msg " ^ message_to_string m
  | Close p -> "close " ^ peer_to_string p
  | Unknown s -> s

let send_command (fd : Lwt_unix.file_descr) (request : peers_request_t) :
    unit Lwt.t =
  let serialized = peers_request_to_string request in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt_io.write_line oc serialized >>= fun () -> Lwt_io.flush oc

let rec send_msg_to_peers msg = function
  | [] -> Lwt.return_unit
  | (fd, host, port) :: tail ->
      send_command fd
        (Msg (Unix.inet_addr_loopback, int_of_string Sys.argv.(1), msg))
      >>= fun () ->
      Logs_lwt.info (fun m ->
          m "%s sending to %s:%d %s" (timestamp ())
            (Unix.string_of_inet_addr host)
            port msg )
      >>= fun () -> send_msg_to_peers msg tail

let rec close_peers = function
  | [] -> Lwt.return_unit
  | (fd, _, _) :: tail ->
      send_command fd
        (Close (fd, Unix.inet_addr_loopback, int_of_string Sys.argv.(1)))
      >>= fun () -> close_peers tail

let send_msg = function
  | "\\q" -> close_peers !peers >>= fun () -> exit 0
  | msg ->
      history :=
        add_message_to_history
          (Unix.inet_addr_loopback, int_of_string Sys.argv.(1), msg)
          !history ;
      send_msg_to_peers msg (List.rev (List.tl (List.rev !peers)))

let rec read_daemon () =
  Lwt_io.read_line_opt Lwt_io.stdin
  >>= function
  | None -> Lwt.return_unit
  | Some line -> send_msg line >>= fun () -> read_daemon ()

let parse_history_request (_ : Lwt_unix.file_descr) (words : string list) :
    peers_request_t =
  match words with
  | _ :: [] -> History
  | _ -> Unknown "\"history\" does not take any arguments"

let parse_request (fd : Lwt_unix.file_descr) (request : string) :
    peers_request_t =
  let words =
    Str.split (Str.regexp "[ \t\n\r\x0c]+") request
    |> List.filter (fun x -> x <> "")
  in
  match words with
  | "join" :: _ -> parse_join_request_peers fd words
  | "msg" :: _ -> parse_msg_request fd words request
  | "history" :: _ -> parse_history_request fd words
  | "close" :: _ -> parse_close_request_peers fd words
  | _ -> Unknown "unknown command or empty"

let execute_request : peers_request_t -> peers_response_t = function
  | Join peer ->
      add_to_peers peer ;
      connect_to_peer_on_join peer |> ignore ;
      OnJoin (List.filter (fun x -> not (is_same_origin x peer)) !peers)
  | History -> OnHistory !history
  | Msg message ->
      history := add_message_to_history message !history ;
      Logs.info (fun m -> m "%s %s" (timestamp ()) (message_to_string message)) ;
      OnMsg
  | Close peer -> remove_from_peers peer ; OnClose
  | Unknown error -> OnUnknown error

let to_string : peers_response_t -> string = function
  | OnJoin peers -> peers_as_string peers
  | OnMsg -> ""
  | OnHistory history -> history_as_string history
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

let create_server_socket (port : int) : Lwt_unix.file_descr =
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Logs.info (fun m ->
      m "%s Starting on host = %s ; port = %d" (timestamp ())
        (Unix.string_of_inet_addr listen_address)
        port ) ;
  Lwt_main.run (Lwt_unix.bind sock (ADDR_INET (listen_address, port))) ;
  Lwt_unix.listen sock backlog_size ;
  sock

let create_server (sock : Lwt_unix.file_descr) : unit -> unit Lwt.t =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let iport = int_of_string Sys.argv.(1) in
  let sock = create_server_socket iport in
  let serve = create_server sock in
  Lwt_main.run (init (Array.to_list Sys.argv)) ;
  Lwt_main.run (Lwt.join [read_daemon (); serve ()])
