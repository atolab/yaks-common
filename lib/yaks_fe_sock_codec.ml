open Apero
open Yaks_types
open Yaks_fe_sock_codes
open Yaks_fe_sock_types

let decode_properties buf =
  decode_string buf
  |> Properties.of_string

let encode_properties props buf =
  encode_string (Properties.to_string props) buf

let encode_header h buf =  
  let id = char_of_int @@ message_id_to_int h.mid in   
  Abuf.write_byte id buf; 
  Abuf.write_byte h.flags buf; 
  encode_vle h.corr_id buf; 
  if h.properties <> Property.Map.empty 
  then encode_properties h.properties buf
  else ()


let decode_header buf =   
  Abuf.read_byte buf 
  |> fun id -> Abuf.read_byte buf 
  |> fun flags -> decode_vle buf 
  |> fun corr_id -> 
    (if has_property_flag flags 
    then decode_properties buf
    else Property.Map.empty)
  |> fun properties -> 
    match int_to_message_id (int_of_char id) with 
    | Some mid -> {mid; flags; corr_id; properties}
    | None -> raise @@ Exception `UnknownMessageId 

let encode_value v buf =
  let open Value in
  let encode_value_encoding e = Abuf.write_byte (char_of_int @@ value_encoding_to_int e) in
  match v with
  | RawValue (descr, b) -> 
    encode_value_encoding RAW buf;
    Apero.encode_string (Option.get_or_default descr "") buf;
    Apero.encode_bytes b buf
  | StringValue s -> 
    encode_value_encoding STRING buf;
    Apero.encode_string s buf
  | PropertiesValue p -> 
    encode_value_encoding PROPERTIES buf;
    Apero.encode_string (Properties.to_string p) buf
  | JSonValue s -> 
    encode_value_encoding JSON buf;
    Apero.encode_string s buf
  | SqlValue (row, col) -> 
    encode_value_encoding SQL buf;
    Apero.encode_seq Apero.encode_string row buf;
    match col with
      | Some col -> Apero.encode_seq Apero.encode_string col buf
      | None -> Apero.encode_seq Apero.encode_string [] buf

let decode_value buf =
  let open Value in
  Abuf.read_byte buf
  |> fun encoding -> match int_to_value_encoding @@ int_of_char encoding with
    | Some RAW -> 
      Apero.decode_string buf
      |> fun descr -> Apero.decode_bytes buf
      |> fun b -> let d = if String.length descr > 0 then Some descr else None in
      RawValue (d, b)
    | Some STRING -> 
      Apero.decode_string buf
      |> fun s -> StringValue s
    | Some PROPERTIES -> 
      Apero.decode_string buf
      |> fun s -> PropertiesValue (Properties.of_string s)
    | Some JSON -> 
      Apero.decode_string buf
      |> fun s -> JSonValue s
    | Some SQL -> 
      (Apero.decode_seq Apero.decode_string buf
      |> fun row -> Apero.decode_seq Apero.decode_string buf
      |> function
        | [] -> SqlValue (row, None)
        | col -> SqlValue (row, Some col))
    | Some _ -> raise @@ Exception(`InvalidFormat (`Msg ("Unkown encoding: "^(int_of_char encoding |> string_of_int))))
    | None -> raise @@ Exception(`InvalidFormat `NoMsg)


let decode_pair decode_fst decode_snd buf = 
  decode_fst buf 
  |> fun fst ->
  decode_snd buf 
  |> fun snd -> (fst, snd)

let encode_pair encode_fst encode_snd (fst, snd) buf = 
  encode_fst fst buf; encode_snd snd buf

let encode_path p = encode_string (Path.to_string p)

let decode_path buf = 
  decode_string buf |> Path.of_string_opt 
  |> function
  | Some path -> path
  | None -> raise @@ Exception(`InvalidFormat (`Msg "Invalid path syntax" ))

let encode_selector s = encode_string (Selector.to_string s)

let decode_selector buf = 
  decode_string buf |> Selector.of_string_opt
  |> function
  | Some s -> s
  | None -> raise @@ Exception(`InvalidFormat (`Msg "Invalid selector format" ))

let encode_pathvaluelist = encode_seq (encode_pair encode_path encode_value)
let decode_pathvaluelist = decode_seq (decode_pair decode_path decode_value)

let encode_pathvaluelist_safe = encode_seq_safe (encode_pair encode_path encode_value)


let decode_body (mid:message_id) (flags:char) (buf: Abuf.t) = 
  let _ = ignore flags in (* in case of further need... *)
  match mid with 
  | LOGIN | LOGOUT | OK   -> YEmpty
  | WORKSPACE | DELETE    -> decode_path buf |> fun path -> YPath path
  | PUT | UPDATE | VALUES -> decode_pathvaluelist buf |> fun pvs -> YPathValueList pvs
  | GET | EVAL | SUB      -> decode_selector buf |> fun sel -> YSelector sel
  | UNSUB                 -> decode_string buf |> fun sid -> YSubscription sid
  | NOTIFY                -> decode_string buf |> fun sid -> decode_pathvaluelist buf |> fun pvs -> YNotification (sid, pvs)
  | REG_EVAL | UNREG_EVAL -> decode_path buf |> fun path -> YPath path
  | ERROR                 -> decode_vle buf |> fun errno -> YErrorInfo errno

let encode_body body buf = 
  match body with 
  | YEmpty                   -> ()
  | YPath p                  -> encode_path p buf
  | YSelector s              -> encode_selector s buf
  | YPathValueList pvs       -> encode_pathvaluelist pvs buf
  | YSubscription s          -> encode_string s buf
  | YNotification (sid, pvs) -> encode_string sid buf; encode_pathvaluelist pvs buf
  | YErrorInfo (code)        -> encode_vle code buf


let decode_message buf = 
  decode_header buf 
  |> fun header ->     
  decode_body header.mid header.flags buf 
  |> fun body -> {header;  body}

let encode_message msg buf =
  encode_header msg.header buf;
  encode_body msg.body buf

let encode_message_split msg buf =
  match msg.body with
  | YPathValueList pvs ->
    let headerpos = Abuf.w_pos buf in
    encode_header msg.header buf;
    encode_pathvaluelist_safe pvs buf
    |> fun remain ->
      if (List.length pvs <> 0) && (List.length remain = List.length pvs) 
      then raise @@ Exception(`IOError (`Msg (Printf.sprintf "Buffer too small (%d bytes) to encode the Value for Path %s"
                    (Abuf.writable_bytes buf) (List.hd pvs |> fun (p,_) -> Path.to_string p))))
      else 
        if List.length remain = 0 
        then None
        else
          (* add INCOMPLE flag in message header *)
          let flags = char_of_int ((int_of_char msg.header.flags) lor (message_flags_to_int INCOMPLETE)) in
          Abuf.set_byte ~at:(headerpos+1) flags buf;
          Some {msg with body=YPathValueList remain}
  | _ -> encode_message msg buf; None
