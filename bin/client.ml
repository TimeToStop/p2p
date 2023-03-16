open Lwt

module Constants = struct
  let timeout = 1.

  let listen_address = Unix.inet_addr_loopback

  let backlog_size = 10
end

let peers = ref []

let history = ref []

let add_message_to_history msg history = msg :: history

let rec history_as_string = function
  | [] -> ""
  | msg :: rest -> history_as_string rest ^ ";" ^ msg

let add_peer peers = function
  | fd, addr, port ->
      peers
      |> List.filter (fun (_, a, ip) -> addr ^ ":" ^ port <> a ^ ":" ^ ip)
      |> List.append [(fd, addr, port)]

let remove_from_peers prs addr port =
  prs |> List.filter (fun (_, a, ip) -> addr ^ ":" ^ port <> a ^ ":" ^ ip)

let rec peers_as_string = function
  | [] -> "\n"
  | (_, host, port) :: rest -> host ^ ":" ^ port ^ " " ^ peers_as_string rest

let send_message_to_peer fd msg =
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt_io.write_line oc msg >>= fun () -> Lwt_io.flush oc

let recieve_response_from_peer fd =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  Lwt_io.read_line_opt ic
  >>= function
  | None -> failwith "expected response"
  | Some response -> Lwt.return response

let connect_to_peer_on_join hostname port =
  let addr = Unix.inet_addr_of_string hostname in
  let iport = int_of_string port in
  let sockaddr = Unix.ADDR_INET (addr, iport) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt_float socket SO_RCVTIMEO Constants.timeout ;
  Lwt_unix.connect socket sockaddr
  >>= fun () ->
  let peer = (socket, hostname, port) in
  peers := add_peer !peers peer ;
  Logs_lwt.info (fun m -> m "Connected to %s:%d" hostname iport)

let connect_to_rest_peer_impl hostname port =
  let addr = Unix.inet_addr_of_string hostname in
  let iport = int_of_string port in
  let sockaddr = Unix.ADDR_INET (addr, iport) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt_float socket SO_RCVTIMEO Constants.timeout ;
  Lwt_unix.connect socket sockaddr
  >>= fun () ->
  Lwt.return (peers := add_peer !peers (socket, hostname, port))
  >>= fun _ ->
  send_message_to_peer socket ("join 127.0.0.1 " ^ Sys.argv.(1))
  >>= fun () ->
  recieve_response_from_peer socket
  >>= fun _ -> Logs_lwt.info (fun m -> m "connect to %s:%d" hostname iport)

let connect_to_rest_peer peer =
  match Str.split (Str.regexp "[:]+") peer with
  | [hostname; port] -> connect_to_rest_peer_impl hostname port
  | _ -> failwith ("bad peer msg: " ^ peer)

let connect_to_peer host port =
  let addr = Unix.inet_addr_of_string host in
  let iport = int_of_string port in
  let sockaddr = Unix.ADDR_INET (addr, iport) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt_float socket SO_RCVTIMEO Constants.timeout ;
  Lwt_unix.connect socket sockaddr
  >>= fun () -> Lwt.return (socket, host, port)

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
        Logs_lwt.info (fun m -> m "%s" head) >>= fun () -> print_msg tail
  in
  print_msg msgs

let connect_via_peer host port =
  connect_to_peer host port
  >>= fun (socket, host, port) ->
  peers := add_peer !peers (socket, host, port) ;
  send_message_to_peer socket ("join 127.0.0.1 " ^ Sys.argv.(1))
  >>= fun () ->
  recieve_response_from_peer socket
  >>= handle_join_response
  >>= fun () ->
  send_message_to_peer socket "history"
  >>= fun () -> recieve_response_from_peer socket >>= handle_history_response

let fetch_history () =
  let sock, _, _ = List.nth !peers 0 in
  send_message_to_peer sock "history"
  >>= fun () ->
  recieve_response_from_peer sock
  >>= fun response ->
  Lwt.return
    (Str.split (Str.regexp "[;]+") response |> List.filter (fun x -> x != ""))

let print_messages msgs =
  let rec print_msg = function
    | [] -> Lwt.return_unit
    | head :: tail ->
        Logs_lwt.info (fun m -> m "%s" head) >>= fun () -> print_msg tail
  in
  print_msg msgs

let connect_via_discovery host port =
  connect_to_peer host port
  >>= fun (socket, host, port) ->
  peers := add_peer !peers (socket, host, port) ;
  send_message_to_peer socket ("join 127.0.0.1 " ^ Sys.argv.(1))
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
            "usage:\n\
             <port> c <hostname> <port> - connect via existing peer\n\
             <port> d <hostname> <port> - connect via discovery server" )

let send_msg_to_peer msg peer =
  let fd, _, _ = peer in
  let packet = "msg " ^ "127.0.0.1 " ^ Sys.argv.(1) ^ " username " ^ msg in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt_io.write_line oc packet >>= fun () -> Lwt_io.flush oc

let rec send_msg_to_peers msg = function
  | [] -> Lwt.return_unit
  | head :: tail ->
      send_msg_to_peer msg head
      >>= fun () ->
      Logs_lwt.info (fun m -> m "sending %s" msg)
      >>= fun () -> send_msg_to_peers msg tail

let close_peer peer =
  let fd, _, _ = peer in
  let packet = "close " ^ "127.0.0.1 " ^ Sys.argv.(1) in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt_io.write_line oc packet
  >>= fun () -> Lwt_io.flush oc >>= fun () -> exit 0

let rec close_peers = function
  | [] -> Lwt.return_unit
  | head :: tail -> close_peer head >>= fun () -> close_peers tail

let send_msg = function
  | "\\q" -> close_peers !peers
  | msg ->
      history :=
        add_message_to_history
          ("[127.0.0.1:" ^ Sys.argv.(1) ^ "] username: " ^ msg)
          !history ;
      send_msg_to_peers msg !peers

let rec read_daemon () =
  Lwt_io.read_line_opt Lwt_io.stdin
  >>= function
  | None -> Lwt.return_unit
  | Some line -> send_msg line >>= fun () -> read_daemon ()

let get_text cmd host port username msg =
  let pos =
    4 + String.length cmd + String.length host + String.length port
    + String.length username
  in
  String.sub msg pos (String.length msg - pos)

let handle_message fd msg =
  match Str.split (Str.regexp "[ \t\n\r\x0c]+") msg with
  | ["join"; host; port] ->
      peers := add_peer !peers (fd, host, port) ;
      connect_to_peer_on_join host port
      >>= fun () ->
      Lwt.return
        (peers_as_string
           (List.filter
              (fun (_, x_host, x_port) ->
                x_host ^ ":" ^ x_port <> host ^ ":" ^ port )
              !peers ) )
  | "msg" :: host :: port :: username :: _ ->
      let text = get_text "msg" host port username msg in
      history :=
        add_message_to_history
          ("[" ^ host ^ ":" ^ port ^ "] " ^ username ^ ": " ^ text)
          !history ;
      Logs_lwt.info (fun m -> m "%s" text)
      >>= fun () -> Lwt.return "accepted"
  | "history" :: [] -> Lwt.return (history_as_string !history)
  | ["close"; host; port] ->
      peers := remove_from_peers !peers host port ;
      Lwt.return "exit"
  | _ -> Lwt.return "Unknown command"

let rec handle_connection fd ic oc () =
  Lwt_io.read_line_opt ic
  >>= fun msg ->
  match msg with
  | Some msg ->
      handle_message fd msg
      >>= fun reply ->
      Logs_lwt.info (fun m -> m "request=%s response=%s " msg reply)
      >>= fun () ->
      Lwt_io.write_line oc reply
      >>= fun () -> Lwt_io.flush oc >>= handle_connection fd ic oc
  | None -> Logs_lwt.info (fun m -> m "Connection closed") >>= return

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt.on_failure (handle_connection fd ic oc ()) (fun e ->
      Logs.err (fun m -> m "%s" (Printexc.to_string e)) ) ;
  Logs_lwt.info (fun m -> m "New connection") >>= return

let create_server_socket port =
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt_float sock SO_RCVTIMEO Constants.timeout ;
  Lwt_main.run
    ( Logs_lwt.info (fun m ->
          m "Starting on host = %s ; port = %d"
            (Unix.string_of_inet_addr Constants.listen_address)
            port )
    >>= return ) ;
  Lwt_main.run
    (Lwt_unix.bind sock (ADDR_INET (Constants.listen_address, port))) ;
  Lwt_unix.listen sock Constants.backlog_size ;
  sock

let create_server sock =
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
