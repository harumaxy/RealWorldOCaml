open Core
open Base

type 'a with_line_num = { item : 'a; line_num : int }

module Heartbeat = struct
  type t = { session_id : string; time : Time_ns.t; status_message : string }
end

type client_info = {
  addr : Unix.Inet_addr.t;
  port : int;
  user : string;
  credentials : string;
  last_heartbeat_time : Time_ns.t;
}

let register_heartbeat t hb = { t with last_heartbeat_time = hb.Heartbeat.time }

module Logon = struct
  type t = {
    session_id : string;
    time : Time_ns.t;
    user : string;
    credentials : string;
  }
  [@@deriving fields]
end

;;
Field.get Logon.Field.user
