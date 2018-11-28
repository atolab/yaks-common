open Yaks_common_errors

module Path : sig
  type t
  val of_string : ?is_absolute:bool -> string -> t
  (** [of_string s] returns [s] as a Path if it's valid. Otherwise it raises a [YException].
      Note that the Path's string is sanitized (i.e. it's trimmed meaningless '/' are removed) *)
  val of_string_opt : ?is_absolute:bool -> string -> t option
  (** [of_string_opt s] returns [Some p] if [s] is a valid path. Otherwise it raises a [YException].
      Note that the Path's string is sanitized (i.e. it's trimmed and meaningless '/' are removed) *)
  val to_string : t -> string
  (** [to_string path] returns the [path] as a string. *)

  val length : t -> int
  (** [length p] returns the number of characters of path [p] *)
  val compare : t -> t -> int
  (** The comparison function for strings, with the same specification as [Pervasives.compare] *)

  val is_prefix : affix:t -> t -> bool
  (** [is_prefix affix p] returns true if [affix] is a prefix of [p] *)
  val remove_prefix : int -> t -> t
  (** [remove_prefix l p] removes the [l] first characters from Path [p] and returns the remaining as a non-absolute Path *)
end [@@deriving show]

module Selector : sig
  type t          
  val of_string : ?is_absolute:bool -> string -> t
  (** [of_string s] validate the format of the string [s] as a selector and returns a Selector if valid.
      If the validation fails, an [YException] is raised. If [is_absolute] is true (default value) the string must start with '/' *)
  val of_string_opt : ?is_absolute:bool -> string -> t option
  (** [of_string_opt s] validate the format of the string [s] as a selector and returns some Selector if valid.
      If the validation fails, None is returned. If [is_absolute] is true (default value) the string must start with '/' *)
  val to_string : t -> string
  (** [to_string s] return the Selector [s] as a string *)
  val of_path : Path.t -> t
  (** [of_path p] returns a Selector with its path equal to [p] and without query and fragment *)

  val get_path : t -> string
  (** [path s] returns the path part of the Selector [s]. I.e. the part before any '?' character. *)
  val get_query : t -> string option
  (** [query s] returns the query part of the Selector [s].
      I.e. the part after the first '?' character and before the fist '#' character, or an empty string if no '?' is found. *)
  val get_fragment : t -> string option
  (** [fragment s] returns the fragment part of the Selector [s].
      I.e. the part after the first '#', or an empty string if no '#' is found. *)

  val is_path_unique : t -> bool
  (** [is_path_unique s] returns true it the path part of Selector [s] doesn't contains any wildcard ('*'). *)
  val as_unique_path : t -> Path.t option
  (** [as_unique_path s] returns the path part of Selector [s] as Some Path if it doesn't contain any wildcard ('*').
      It returns None otherwise. *)

  val is_matching_path : Path.t -> t -> bool
  (** [is_matchind_path p s] returns true if the selector [s] fully matches the path [p].
      Note that only the path part of the selector is considered for the matching (not the query and the fragment) *)
  val is_prefixed_by_path : Path.t -> t -> bool
  (** [is_prefixed_by_path p s] returns true if the selector [s] partially matches the path [p].
      I.e. it exists a prefix of [s] that fully matches [p].
      Note that only the path part of the selector is considered for the matching (not the query and the fragment) *)
  val remove_matching_prefix : Path.t -> t -> t option
  (** [remove_matchin_prefix p s] checks if the Path [p] matches a substring prefixing the Selector [s].
      If there is such matching prefix in [s], a similar Selector than [s] is returned, but with this prefix removed
      from its path part. If there is no such matching, None is returned. *)
end [@@deriving show]

module Value : sig 
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


  val update : t -> t -> (t, yerror) Apero.Result.t
  val encoding : t -> encoding
  val encoding_to_string : encoding -> string
  val encoding_of_string : string -> encoding
  val transcode : t -> encoding -> (t, yerror) Apero.Result.t   
  val of_string : string -> encoding -> (t, yerror) Apero.Result.t 
  val to_string : t -> string
end

module AccessId : (module type of Apero.Uuid)
module StorageId : (module type of Apero.Uuid)
module SubscriberId : Apero.Id.S