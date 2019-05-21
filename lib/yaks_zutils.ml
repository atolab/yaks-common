open Apero
open Yaks_types
open Yaks_common_errors

let kind_put = 0L
let kind_update = 1L
let kind_remove = 2L

let zenohid_to_yaksid id = Printf.sprintf "%s-%s-%s-%s-%s" 
  (Astring.with_index_range ~first:0 ~last:7 id)
  (Astring.with_index_range ~first:8 ~last:11 id)
  (Astring.with_index_range ~first:12 ~last:15 id)
  (Astring.with_index_range ~first:16 ~last:19 id)
  (Astring.with_index_range ~first:20 ~last:31 id)

let encoding_to_flag v = Value.encoding v |> Value.encoding_to_int |> Int64.of_int
let encoding_of_flag f = match f with 
  | None -> Value.RAW 
  | Some i -> Option.get_or_default (Value.int_to_encoding (Int64.to_int i)) Value.RAW

let encode_value v buf = match Value.transcode v Value.RAW with
  | Ok Value.RawValue(_, b) -> Apero.encode_bytes b buf
  | Ok v' -> Logs.err (fun m -> m "[YZu]: INTERNAL ERROR: transcode of value '%s' to RAW didn't return a RawValue but: '%s'" (Value.to_string v) (Value.to_string v'))
  | Error e -> Logs.err (fun m -> m "[YZu]: INTERNAL ERROR: transcode of value '%s' to RAW failed: '%s'" (Value.to_string v) (show_yerror e))
let decode_value buf encoding =
  let raw_value = Value.RawValue(None, Apero.decode_bytes buf) in
  match Value.transcode raw_value encoding with
  | Ok v -> v
  | Error e -> raise @@ YException e

let timestamp0 = Timestamp.create 
  (Option.get @@ Uuid.of_string "00000000-0000-0000-0000-000000000000")
  (Option.get @@ Time.of_string "0")

let decode_time ?hlc (info:Ztypes.data_info) = match (info.ts, hlc) with 
  | (Some t, _) -> Lwt.return t 
  | (None, Some hlc) -> Logs.warn (fun m -> m "Received a data from Zenoh without timestamp; generate it");
    HLC.new_timestamp hlc
  | (None, None) -> Logs.warn (fun m -> m "Received a data from Zenoh without timestamp; set time to 0");
    Lwt.return timestamp0

let decode_timedvalue ?hlc (buf:Abuf.t) (info:Ztypes.data_info) =
  let%lwt time = decode_time ?hlc info in
  let encoding = encoding_of_flag info.encoding in
  let value = decode_value buf encoding in
  let (tv:TimedValue.t) = { time; value } in
  Lwt.return tv

let decode_change ?hlc (buf:Abuf.t) (info:Ztypes.data_info) =
  let open Lwt.Infix in
  match Option.get_or_default info.kind kind_put with
  | k when k=kind_put    -> decode_timedvalue ?hlc buf info >|= fun tv -> Put tv
  | k when k=kind_update -> decode_timedvalue ?hlc buf info >|= fun tv -> Update tv
  | k when k=kind_remove -> decode_time ?hlc info  >|= fun time -> Remove time
  | k -> Lwt.fail @@ YException (`InternalError (`Msg ("Unkown kind value in incoming Zenoh message: "^(Int64.to_string k))))

let decode_changes ?hlc samples =
  List.map (fun (buf, (info:Ztypes.data_info)) -> decode_change ?hlc buf info) samples |>
  Lwt_list.fold_left_s (fun acc lwt ->
    (* drop the failing Lwt.t (e.g. decoding failure), logging an error for each *)
    Lwt.try_bind (fun () -> lwt) (fun x -> Lwt.return @@ x::acc)
    (fun e -> Logs.err (fun m -> m "[YZu]: INTERNAL ERROR receiving data via Zenoh: %s" (Printexc.to_string e)); Lwt.return acc)
  ) []

let query_timedvalues zenoh hlc selector =
  let open Lwt.Infix in
  let reply_to_ktv (resname, buf, (info:Ztypes.data_info)) =
    let path = Path.of_string resname in
    let%lwt timedvalue = decode_timedvalue ~hlc buf info in
    Lwt.return (path, timedvalue)
  in
  let resname = Selector.path selector in
  let predicate = Selector.optional_part selector in
  Zenoh.lquery zenoh resname predicate
  >>= Lwt_list.map_p reply_to_ktv

let query_values zenoh selector =
  let open Lwt.Infix in
  let reply_to_kv (resname, buf, (info:Ztypes.data_info)) =
    let path = Path.of_string resname in
    let encoding = encoding_of_flag info.encoding in
    let value = decode_value buf encoding in
    (path, value)
  in
  let resname = Selector.path selector in
  let predicate = Selector.optional_part selector in
  Zenoh.lquery zenoh resname predicate
  >|= List.map reply_to_kv

let write_put zenoh ?timestamp path (value:Value.t) =
  let res = Path.to_string path in
  let buf = Abuf.create ~grow:8192 8192 in
  let encoding = encoding_to_flag value in
  encode_value value buf;
  Zenoh.write zenoh res ?timestamp ~encoding buf

let write_update zenoh ?timestamp path (value:Value.t) =
  let res = Path.to_string path in
  let buf = Abuf.create ~grow:8192 8192 in
  let encoding = encoding_to_flag value in
  encode_value value buf;
  Zenoh.write zenoh res ?timestamp ~encoding ~kind:kind_update buf

let empty_buf = Abuf.create 0
let write_remove zenoh ?timestamp  path =
  let res = Path.to_string path in
  Zenoh.write zenoh res ?timestamp ~kind:kind_remove empty_buf


let subscribe zenoh ?hlc ?listener selector =
  let open Lwt.Infix in
  let (zmode, zlistener) = match listener with
    | None -> (Zenoh.pull_mode, fun _ _ -> Lwt.return_unit)
    | Some callback -> 
      (Zenoh.push_mode, 
      fun resname samples -> match Path.of_string_opt resname with
        | Some path -> decode_changes ?hlc samples >>= callback path
        | None -> Logs.err (fun m -> m "[YZu]: Subscriber received data via Zenoh for an invalid path: %s" resname); Lwt.return_unit
      )
  in
  Zenoh.subscribe zenoh (Selector.to_string selector) zlistener ~mode:zmode

let unsubscribe = Zenoh.unsubscribe
