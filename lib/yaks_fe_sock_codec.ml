open Apero
open Apero.Result.Infix
open Yaks_common_types
open Yaks_fe_sock_codes
open Yaks_fe_sock_types

let encode_property k v buf =
  encode_string k buf
  >>= fun buf ->
  encode_string v buf

let decode_property  buf = 
  decode_string buf 
  >>= fun (k, buf) -> 
  decode_string buf 
  >>= fun (v, buf) -> Result.ok (Property.make k v, buf)

let decode_properties buf =
  let rec get_remaining props length buf =
    let open Result in
    match length with
    | 0 -> return (props, buf)
    | _ ->
      decode_property buf 
      >>= (fun ((k,v), buf) -> get_remaining (Property.Map.add k v props) (length - 1) buf)
  in
  decode_vle buf
  >>= (fun (length, buf) ->    
      (get_remaining Property.Map.empty (Vle.to_int length) buf))

let encode_properties props buf =
  (encode_vle (Vle.of_int (Property.Map.cardinal props)) buf)
  |> Property.Map.fold (fun k v res -> match res with
      | Ok buf -> encode_property k v buf
      | Error e -> Error e) props


let encode_header h buf =  
  let id = char_of_int @@ message_id_to_int h.mid in   
  IOBuf.put_char id buf 
  >>= fun buf -> 
  IOBuf.put_char h.flags buf 
  >>= fun buf ->
  encode_vle h.corr_id buf 
  >>= fun buf -> 
  if h.properties <> Property.Map.empty then 
    encode_properties h.properties buf
  else Result.ok buf


let decode_header buf =   
  IOBuf.get_char buf 
  >>= fun (id, buf) -> IOBuf.get_char buf 
  >>= fun (flags, buf) -> decode_vle buf 
  >>= fun (corr_id, buf) -> 
  (if has_property_flag flags then decode_properties buf
   else 
     let ps = Property.Map.empty in Result.ok (ps, buf) )
  >>= fun (properties, buf) -> 
  match int_to_message_id (int_of_char id) with 
  | Some mid -> Result.ok ({mid; flags; corr_id; properties}, buf)
  | None -> Result.fail `UnknownMessageId 

let encode_value v buf =
  let open Value in
  let encode_value_encoding e = IOBuf.put_char (char_of_int @@ value_encoding_to_int e) buf in
  match v with
  | RawValue b -> encode_value_encoding RAW
    >>= fun buf -> Apero.encode_bytes (IOBuf.from_bytes b) buf
  | StringValue s -> encode_value_encoding STRING
    >>= fun buf -> Apero.encode_string s buf
  | JSonValue s -> encode_value_encoding JSON
    >>= fun buf -> Apero.encode_string s buf
  | SqlValue (row, col) -> encode_value_encoding SQL
    >>= fun buf -> Apero.encode_seq Apero.encode_string row buf
    >>= (fun buf -> match col with
        | Some col -> Apero.encode_seq Apero.encode_string col buf
        | None -> Apero.encode_seq Apero.encode_string [] buf)

let decode_value buf =
  let open Value in
  IOBuf.get_char buf
  >>= fun (encoding, buf) -> match int_to_value_encoding @@ int_of_char encoding with
    | Some RAW -> Apero.decode_bytes buf
      >>= fun (b, buf) -> Result.ok (RawValue (IOBuf.to_bytes b), buf)
    | Some STRING -> Apero.decode_string buf
      >>= fun (s, buf) -> Result.ok (StringValue s, buf)
    | Some JSON -> Apero.decode_string buf
      >>= fun (s, buf) -> Result.ok (JSonValue s, buf)
    | Some SQL -> Apero.decode_seq Apero.decode_string buf
      >>= fun (row, buf) -> Apero.decode_seq Apero.decode_string buf
      >>= (fun (col, buf) -> match col with
          | [] -> Result.ok (SqlValue (row, None), buf)
          | _  -> Result.ok (SqlValue (row, Some col), buf))
    | Some _ -> Result.fail @@ `InvalidFormat (`Msg ("Unkown encoding: "^(int_of_char encoding |> string_of_int)))
    | None -> Result.fail @@ `InvalidFormat `NoMsg


let decode_pair decode_fst decode_snd buf = 
  decode_fst buf 
  >>= fun (fst, buf) ->
  decode_snd buf 
  >>= fun (snd, buf) -> Result.ok ((fst, snd), buf)  

let encode_pair encode_fst encode_snd (fst, snd) buf = 
  encode_fst fst buf >>= encode_snd snd 

let encode_path p = encode_string (Path.to_string p)

let decode_path buf = 
  decode_string buf 
  >>= fun (p, buf) -> 
  match (Path.of_string_opt p) with 
  | Some path -> Result.ok (path, buf)
  | None -> Result.fail  @@ `InvalidFormat (`Msg "Invalid path syntax" )

let encode_selector s = encode_string (Selector.to_string s)

let decode_selector buf = 
  decode_string buf 
  >>= fun (s, buf) -> 
  match (Selector.of_string_opt s) with 
  | Some s -> Result.ok (s, buf)
  | None ->       
    Result.fail  @@ `InvalidFormat (`Msg "Invalid selector format" )

let encode_pathvaluelist = encode_seq (encode_pair encode_path encode_value)
let decode_pathvaluelist = decode_seq (decode_pair decode_path decode_value)


let decode_body (mid:message_id) (flags:char) (buf: IOBuf.t) = 
  match mid with 
  | OPEN | OK ->
    Ok (YEmpty, buf)
  | CREATE ->
    decode_path buf >>> fun (path, buf) -> YPath path, buf
  | DELETE ->
    (match (has_access_flag flags, has_storage_flag flags) with 
     | (true, false) | (false, true) -> Ok (YEmpty, buf)  
     | (false, false) -> decode_path buf >>> fun (path, buf) -> YPath path, buf
     | _ -> Result.fail `InvalidFlags)  
  | PUT | PATCH | VALUES ->
    decode_pathvaluelist buf >>> fun (pvs, buf) -> YPathValueList pvs, buf
  | GET | SUB ->
    decode_selector buf >>> fun (sel, buf) -> YSelector sel, buf  
  | UNSUB ->
    decode_string buf >>> fun (sid, buf) -> YSubscription sid, buf
  | NOTIFY ->
    decode_string buf >>= fun (sid, buf) ->
    decode_pathvaluelist buf >>> fun (pvs, buf) -> YNotification (sid, pvs), buf
  | EVAL ->
    Result.fail `NotImplemented
  | ERROR ->
    decode_vle buf >>> fun (errno, buf) -> YErrorInfo errno, buf

let encode_body body buf = 
  match body with 
  | YEmpty -> Ok buf
  | YPath p -> encode_path p buf
  | YSelector s -> encode_selector s buf
  | YPathValueList  pvs -> encode_pathvaluelist pvs buf
  | YSubscription s -> encode_string s buf
  | YNotification (sid, pvs) -> encode_string sid buf >>= encode_pathvaluelist pvs
  | YErrorInfo (code) -> encode_vle code buf


let decode_message buf = 
  let open Result.Infix in 
  decode_header buf 
  >>= fun (header, buf) ->     
  decode_body header.mid  header.flags buf 
  >>= fun (body, buf) -> Result.ok ({header;  body}, buf)

let encode_message msg buf =
  encode_header msg.header buf
  >>= encode_body msg.body
