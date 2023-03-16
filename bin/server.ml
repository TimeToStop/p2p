open Lwt

let listen_address = Unix.inet_addr_loopback

let port = 9002

let backlog = 10

let peers = ref []

let add_to_peers (fd, addr, port) =
  peers :=
    List.filter (fun (_, a, ip) -> addr ^ ":" ^ port <> a ^ ":" ^ ip) !peers
    |> List.append [(fd, addr, port)]

let remove_from_peers prs addr port =
  peers :=
    List.filter (fun (_, a, ip) -> addr ^ ":" ^ port <> a ^ ":" ^ ip) prs

let rec peers_as_string = function
  | [] -> "\n"
  | (_, host, port) :: rest -> host ^ ":" ^ port ^ " " ^ peers_as_string rest

let join_peer fd host port =
  add_to_peers (fd, host, port) ;
  peers_as_string
    (List.filter
       (fun (_, x_host, x_port) -> x_host ^ ":" ^ x_port <> host ^ ":" ^ port)
       !peers )

let close_peer host port =
  remove_from_peers !peers host port ;
  "exit"

let handle_unknown_command () = "Unknown command"

let handle_message fd msg =
  match Str.split (Str.regexp "[ \t\n\r\x0c]+") msg with
  | ["join"; host; port] -> join_peer fd host port
  | ["close"; host; port] -> close_peer host port
  | _ -> handle_unknown_command ()

let rec handle_connection fd ic oc () =
  Lwt_io.read_line_opt ic
  >>= function
  | Some msg ->
      let reply = handle_message fd msg in
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

let create_server_socket () =
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_main.run
    ( Logs_lwt.info (fun m ->
          m "Starting on host = %s ; port = %d"
            (Unix.string_of_inet_addr listen_address)
            port )
    >>= return ) ;
  Lwt_main.run (Lwt_unix.bind sock (ADDR_INET (listen_address, port))) ;
  Lwt_unix.listen sock backlog ;
  sock

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

let () =
  Logs.set_reporter (Logs.format_reporter ()) ;
  Logs.set_level (Some Logs.Info) ;
  let sock = create_server_socket () in
  let serve = create_server sock in
  Lwt_main.run (serve ())
