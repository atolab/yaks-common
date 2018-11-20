open Apero
open Yaks_common_errors


let remove_useless_slashes s =
  if String.length s <= 2 then s
  else
    let buf = Buffer.create (String.length s) in
    let rec filter i =
      if i < String.length s then
        let c = String.get s i in
        if c <> '/' || (i+1 < String.length s && (String.get s (i+1) <> '/')) then
          Buffer.add_char buf c;
        filter (i+1)
    in
    let _ =
      (* Note: add 1st char anyway to preserve the starting // *)
      Buffer.add_char buf (String.get s 0);
      filter 1
    in
    Buffer.contents buf


module Path = struct
  type t = string
  (* from https://tools.ietf.org/html/rfc3986#appendix-B :
     the path of a URI contains any char but '?' or '#'.
     We add '*' as forbidden char *)
  let path_regex = Str.regexp "^[^?#*]+$"

  let is_valid s = Str.string_match path_regex s 0

  let of_string_opt ?(is_absolute=true) s =
    if Astring.length s > 1 && is_valid s then
      if is_absolute && not (Astring.is_prefix ~affix:"//" s) then None
      else Some (remove_useless_slashes s)
    else None

  let of_string ?(is_absolute=true) s =
    Apero.Option.get_or_else (of_string_opt ~is_absolute s)
    (fun () -> raise (YException (`InvalidPath (`Msg s))))

  let to_string s = s

  let compare = String.compare

  (* let matches _ _ = true *)
end [@@deriving show]

module Selector = struct

  type t = { path: string; query: string option; fragment: string option }


  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let sel_regex = Str.regexp "^\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?$"

  let is_valid s = Str.string_match sel_regex s 0

  (* let key s = s.key *)

  let of_string_opt ?(is_absolute=true) s =
    if Astring.length s > 1 && is_valid s then
      if is_absolute && not (Astring.is_prefix ~affix:"//" s) then None
      else 
        let path = remove_useless_slashes @@ Str.matched_group 1 s
        and query = try Some(Str.matched_group 3 s) with Not_found -> None
        and fragment = try Some(Str.matched_group 5 s) with Not_found -> None
        in
        Some { path; query; fragment }
    else None

  let of_string ?(is_absolute=true) s =
    Apero.Option.get_or_else (of_string_opt ~is_absolute s)
    (fun () -> raise (YException (`InvalidPath (`Msg s))))


  let to_string s =
    Printf.sprintf "%s%s%s" s.path 
      (match s.query with | Some(q) -> "?"^q | None -> "")
      (match s.fragment with | Some(f) -> "#"^f | None -> "")

  let get_path s = s.path

  let get_query s = s.query

  let get_fragment s = s.fragment
end


module Value = struct 
  type encoding = 
    | Raw_Encoding
    | String_Encoding 
    | Json_Encoding  
    | Sql_Encoding  

  type sql_row = string list
  type sql_column_names = string list

  type t  = 
    | RawValue of Lwt_bytes.t 
    | StringValue of string
    | JSonValue of string
    | SqlValue of (sql_row * sql_column_names option)

  let update _ _ = Apero.Result.fail `UnsupportedOperation

  let encoding = function 
    | RawValue _ -> Raw_Encoding
    | StringValue _ -> String_Encoding
    | JSonValue _ -> Json_Encoding
    | SqlValue _ -> Sql_Encoding

  let encoding_to_string = function 
    | Raw_Encoding -> "RAW"
    | String_Encoding -> "STRING"
    | Json_Encoding -> "JSON"
    | Sql_Encoding -> "SQL"

  let encoding_of_string s =
    if s = "STRING" then String_Encoding
    else if s = "JSON" then Json_Encoding
    else if s = "SQL" then Sql_Encoding
    else Raw_Encoding

  let sql_val_sep = ',' (* Char.chr 31 *) (* US - unit separator *)
  let sql_val_sep_str = String.make 1 sql_val_sep
  let sql_row_sep = Char.chr 30 (* RS - record separator *)
  let sql_row_sep_str = String.make 1 sql_row_sep
  

  let sql_to_string = function
    | (row, None) -> String.concat sql_val_sep_str row
    | (row, Some col) -> (String.concat sql_val_sep_str row)^sql_row_sep_str^(String.concat sql_val_sep_str col)

  let sql_of_string s = 
    let string_to_list s = String.split_on_char sql_val_sep s |> List.map String.trim in
    match String.split_on_char sql_row_sep s with
    | row::[] -> string_to_list row , None
    | row::col::[] -> string_to_list row , Some (String.split_on_char sql_val_sep col)
    | _ -> raise @@ YException (`UnsupportedTranscoding (`Msg ("String to SQL of  "^s)))

  let to_raw_encoding = function
    | RawValue _ as v -> Apero.Result.ok @@ v
    | StringValue s -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string s)
    | JSonValue s -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string s)
    | SqlValue v  -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string @@ sql_to_string v)

  let to_string_encoding = function 
    | RawValue r  -> Apero.Result.ok @@ StringValue (Lwt_bytes.to_string r)
    | StringValue _ as v  -> Apero.Result.ok @@ v
    | JSonValue s -> Apero.Result.ok @@ StringValue s
    | SqlValue v -> Apero.Result.ok @@ StringValue (sql_to_string v)

  let json_from_sql (row, col) =
    let open Yojson.Basic in
    let kv_list = match col with
    | None -> List.mapi (fun i v -> "'col_"^(string_of_int i) , `String v ) row
    | Some col -> List.map2 (fun k v -> k , `String v) col row
    in
    to_string (`Assoc kv_list)

  let to_json_encoding = 
    let open Yojson.Basic in
    function
    | RawValue r  -> Apero.Result.ok @@ JSonValue (to_string @@ `String (Lwt_bytes.to_string r))  (* @TODO: base-64 encoding? *)
    | StringValue s  -> Apero.Result.ok @@ JSonValue (to_string @@ `String s)
    | JSonValue _ as v -> Apero.Result.ok @@ v
    | SqlValue v -> Apero.Result.ok @@ StringValue (json_from_sql v)

  (* @TODO: use Error instead of Exception *)
  let sql_from_json json =
    let open Yojson.Basic in
    match from_string json with
    | `Assoc l -> List.split l |> fun (col, row) -> (List.map (fun json -> to_string json) row), Some col
    | _ -> raise @@ YException (`UnsupportedTranscoding (`Msg ("Json to SQL of  "^json)))

  let to_sql_encoding = function
    | RawValue r -> Apero.Result.ok @@ SqlValue (sql_of_string (Lwt_bytes.to_string r))
    | StringValue s  -> Apero.Result.ok @@ SqlValue (sql_of_string s)
    | JSonValue s -> Apero.Result.ok @@ SqlValue (sql_from_json s)
    | SqlValue _ as v -> Apero.Result.ok @@ v

  let transcode v = function   
    | Raw_Encoding -> to_raw_encoding v
    | String_Encoding -> to_string_encoding v
    | Json_Encoding -> to_json_encoding v
    | Sql_Encoding -> to_sql_encoding v

  let of_string s e = transcode (StringValue s)  e
  let to_string  = function 
    | RawValue r -> Lwt_bytes.to_string r
    | StringValue s -> s 
    | JSonValue j -> j 
    | SqlValue s -> sql_to_string s

end
module AccessId = Apero.Uuid
module StorageId = Apero.Uuid
module SubscriberId = Id.Make (Int64)
