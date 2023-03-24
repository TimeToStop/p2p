type peer_t = Lwt_unix.file_descr * Unix.inet_addr * int

type message_t = Unix.inet_addr * int * string

type peers_request_t =
  | Join of peer_t
  | History
  | Msg of message_t
  | Close of peer_t
  | Unknown of string

type peers_response_t =
  | OnJoin of peer_t list
  | OnHistory of message_t list
  | OnMsg
  | OnClose
  | OnUnknown of string

type discovery_request_t =
  | Join of peer_t
  | Close of peer_t
  | Unknown of string

type discovery_response_t =
  | OnJoin of peer_t list
  | OnClose
  | OnUnknown of string

let is_same_origin (a : peer_t) (b : peer_t) : bool =
  let _, addr_a, port_a = a in
  let _, addr_b, port_b = b in
  addr_a = addr_b && port_a = port_b

let peer_to_string : peer_t -> string = function
  | _, addr, port -> Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let rec peers_as_string : peer_t list -> string = function
  | [] -> "\n"
  | peer :: rest -> peer_to_string peer ^ " " ^ peers_as_string rest
