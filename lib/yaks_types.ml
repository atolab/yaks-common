open Apero
open Yaks_common_errors


let remove_useless_slashes =
  let slashes = Str.regexp "/+" in
  Str.global_replace slashes "/"

module Path = struct
  type t = string
  (* from https://tools.ietf.org/html/rfc3986#appendix-B :
     the path of a URI contains any char but '?' or '#'.
     We add '*' as forbidden char *)
  let path_regex = Str.regexp "^[^?#*]+$"

  let is_valid s = Str.string_match path_regex s 0

  let of_string_opt ?(is_absolute=true) s =
    let s = Astring.trim s in
    if Astring.length s > 1 && is_valid s then
      if is_absolute && not (Astring.is_prefix ~affix:"/" s) then None
      else Some (remove_useless_slashes s)
    else None

  let of_string ?(is_absolute=true) s =
    Apero.Option.get_or_else (of_string_opt ~is_absolute s)
    (fun () -> raise (YException (`InvalidPath (`Msg s))))

  let to_string s = s

  let length = Astring.length

  let compare = Astring.compare

  let is_prefix ~affix path = Astring.is_prefix ~affix:(to_string affix) (to_string path)

  let remove_prefix length path = Astring.after length (to_string path) |> of_string ~is_absolute:false
end [@@deriving show]

module Selector = struct

  type t = { path: string; query: string option; fragment: string option }


  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let sel_regex = Str.regexp "^\\([^?#]+\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?$"

  let is_valid s = Str.string_match sel_regex s 0

  let of_string_opt ?(is_absolute=true) s =
    let s = Astring.trim s in
    if (not @@ Astring.is_empty s) && is_valid s then
      if is_absolute && not (Astring.is_prefix ~affix:"/" s) then None
      else 
        let query = try Some(Str.matched_group 3 s) with Not_found -> None
        and fragment = try Some(Str.matched_group 5 s) with Not_found -> None
        and path = remove_useless_slashes @@ Str.matched_group 1 s
        (* Note: path computed at the end as remove_useless_slashes also uses Str and will make Str.matched_group misbehave *)
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

  let of_path p = of_string @@ Path.to_string p

  let get_path s = s.path

  let get_query s = s.query

  let get_fragment s = s.fragment

  let is_path_unique sel = not @@ Astring.contains '*' (get_path sel)

  let as_unique_path sel = if is_path_unique sel then Some (Path.of_string @@ get_path sel) else None

  let simple_wildcard = Astring.Sub.v "*"
  let double_wildcard = Astring.Sub.v "**"
  let non_wildcard c = c != '*'
  let is_slash c = c = '/'

  let get_prefix_before_wildcard s = Astring.Sub.take ~sat:non_wildcard s

  let get_prefix_before_wildcard_until_slash s = 
    let open Astring.Sub in 
    let prefix = take ~sat:non_wildcard s in
    match find is_slash prefix with
    | Some slash -> with_index_range ~first:(start_pos prefix) ~last:(start_pos slash) @@ base prefix
    | None -> prefix

  let get_prefix_until_slash s =
    let open Astring.Sub in 
    match find is_slash s with
    | Some slash -> with_index_range ~first:(start_pos s) ~last:(start_pos slash) @@ base s
    | None -> s

  let is_matching_path path selector =
    let open Astring.Sub in
    let sel_path = v @@ get_path selector in
    let path = v @@ Path.to_string path in
    let rec check_matching pat sel =
      (* if selector is empty, path must be emptu too *)
      if is_empty sel then is_empty pat
      (* if selector starts with '**' *)
      else if is_prefix ~affix:double_wildcard sel then
        (* if selector is only "**" it matches all *)
        if length sel <= 2 then true
        else (
          (* get the next wildcard-free sub-string from selector and find it in path *)
          let sub = with_range ~first:2 sel |> get_prefix_before_wildcard in
          match find_sub ~rev:true ~sub pat with
          | None -> false
          | Some sub' ->
            (* substring found; go on with remaining parts *)
            let pat_tail = with_range ~first:(stop_pos sub') path in
            let sel_tail = with_range ~first:(2+(length sub)) sel in
            check_matching pat_tail sel_tail)
      (* if selector starts with '*' *)
      else if is_prefix ~affix:simple_wildcard sel then
        (* if selector is only '*', it matches path if doesn't contain a '/' *)
        if length sel <= 1 then not @@ exists is_slash pat
        else (
          (* get the next wildcard-free and slash-free sub-string from selector and find it in next path' segment *)
          let sub_sel = with_range ~first:1 sel |> get_prefix_before_wildcard_until_slash in
          let sub_pat = get_prefix_until_slash pat in
          match find_sub ~sub:sub_sel sub_pat with
          | None -> false
          | Some sub' ->
            (* substring found; go on with remaining parts *)
            let pat_tail = with_range ~first:(stop_pos sub') path in
            let sel_tail = with_range ~first:(1+(length sub_sel)) sel in
            check_matching pat_tail sel_tail)
      (* selector doesn't start with wildcard *)
      else
        (* get the next wildcard-free sub-string from selector and check is path starts with this *)
        let sub = get_prefix_before_wildcard sel in
        if is_prefix ~affix:sub pat then
          (* path starts with substring; go on with remaining parts *)
          let first = length sub in
          check_matching (with_range ~first pat) (with_range ~first sel)
        else false
    in 
    let result = check_matching path sel_path in
    let _ = Logs_lwt.debug (fun m -> m "[Yco] Selector.is_matching_path %a %a : %b" pp path pp sel_path result) in
    result

  let remove_matching_prefix path selector =
    let open Astring.Sub in
    let sel_path = v @@ get_path selector in
    let path = v @@ Path.to_string path in
    let _ = Logs_lwt.debug (fun m -> m "---- is_prefixed_by_path %a %a " pp path pp sel_path) in
    let rec check_is_prefixed pat sel =
      (* if path is empty, it's indeeed a prefix of the selector, return the selector *)
      if is_empty pat then Some sel
      (* if selector is empty, there is no match, return None *)
      else if is_empty sel then None
      (* if selector starts with '**', the path is indeed matching '**', return the selctor (incl. '**') *)
      else if is_prefix ~affix:double_wildcard sel then Some(sel)
      (* if selector starts with '*' *)
      else if is_prefix ~affix:simple_wildcard sel then
        (* if path has no '/', '*' match the path, return the remaining of selector after its '*' *)
        if not @@ exists is_slash pat then Some(with_range ~first:1 sel)
        (* else, if the selector is only '*', the path is not matching *)
        else if length sel <= 1 then None
        (* else, go on after consumption of '*' and the matching segment in path *)
        else (
          let sub_sel = with_range ~first:1 sel |> get_prefix_before_wildcard_until_slash in
          let sub_pat = get_prefix_until_slash pat in
          match find_sub ~sub:sub_sel sub_pat with
          | None -> None
          | Some sub' ->
            let pat_tail = with_range ~first:(stop_pos sub') path in
            let sel_tail = with_range ~first:(1+(length sub_sel)) sel in
            check_is_prefixed pat_tail sel_tail)
      (* if selector doesn't start with wildcard, check if it's part before the first wildcard matches the path *)
      else
        let sub = get_prefix_before_wildcard sel in
        if is_prefix ~affix:pat sub then Some(with_range ~first:(length pat) sel)
        else if is_prefix ~affix:sub pat then
          let first = length sub in
          check_is_prefixed (with_range ~first pat) (with_range ~first sel)
        else None
    in 
    let open Apero.Option.Infix in
    check_is_prefixed path sel_path >|= to_string >>= of_string_opt ~is_absolute:false


  let is_prefixed_by_path path selector = remove_matching_prefix path selector |> Apero.Option.is_some

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
