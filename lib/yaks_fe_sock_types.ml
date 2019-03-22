open Apero
open Yaks_types
open Yaks_fe_sock_codes

(* The structure of a socket front end message is the following.

      7 6 5 4 3 2 1 0
    +-+-+-+-+-+-+-+-+  ---+
    |  MESSAGE CODE |     |
    +-+-+-+-+-+-+-+-+     |
    |X|X|X|X|X|X|X|P|     +--> Header
    +-+-+-+-+-+-+-+-+     | 
    ~   Coor. ID    ~     |
    +---------------+     |
    ~   Properties  ~     +-> Present if P = 1
    +---------------+-----+
    ~     Body      ~ --> its structure depends on the message code
    +---------------+   

   For transports that do not preserve message boundaries, the framing is done by prepending
   the lenght encoded using VLE.  
   
   Values are encoded as follows:

    +-+-+-+-+-+-+-+-+
    |    Encoding   |
    +-+-+-+-+-+-+-+-+
    ~    Length     ~
    +-+-+-+-+-+-+-+-+
    ~      Data     ~
    +---------------+

   *)

type header = { 
  mid : message_id;
  flags : char;
  corr_id : Vle.t;  
  properties : properties;
}

let max_msg_size  = 1024 * 64 

let make_header mid (mflags: message_flags list) corr_id properties = 
  let base_flags = List.fold_left (fun a f -> a lor (message_flags_to_int f)) 0 mflags in 
  let flags = char_of_int @@ match Property.Map.is_empty properties with 
    | true ->  base_flags
    | false -> (message_flags_to_int PROPERTY) lor base_flags
  in 
  {mid; flags; corr_id; properties}

let has_property_flag flags = (int_of_char flags) land (message_flags_to_int PROPERTY) <> 0
let has_incomplete_flag flags = (int_of_char flags) land (message_flags_to_int INCOMPLETE) <> 0

type payload = 
  | YEmpty
  | YPath of Path.t
  | YSelector of Selector.t
  | YPathValueList of (Path.t * Value.t) list
  | YSubscription of string
  | YNotification of string * (Path.t * change) list
  | YErrorInfo of Vle.t

let error_info_to_string vle =
  let code = Vle.to_int vle in
  match int_to_error_code code with 
  | Some c -> error_code_to_string c | None -> "UNKOWN ("^(string_of_int code)^")"


type message = {
  header: header;  
  body : payload 
}

let make_message header body = {header; body} 

let get_empty_payload msg = 
  match msg.body with 
  | YEmpty -> Some ()
  | _ -> None

let get_path_payload msg = 
  match msg.body with 
  | YPath p -> Some p 
  | _ -> None 

let get_selector_payload msg = 
  match msg.body with 
  | YSelector s -> Some s
  | _ -> None 

let get_path_value_list_payload msg = 
  match msg.body with  
  | YPathValueList pvs -> Some pvs
  | _ -> None 

let get_subscription_payload msg = 
  match msg.body with  
  | YSubscription s -> Some s
  | _ -> None 

let get_error_info msg =
  match msg.body with
  | YErrorInfo code -> int_to_error_code @@ Vle.to_int code
  | _ -> None
