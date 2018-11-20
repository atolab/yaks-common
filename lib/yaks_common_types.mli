open Yaks_common_errors

module Path : sig
  type t
  val of_string : ?is_absolute:bool -> string -> t
  (** [of_string s] returns [s] as a Path if it's valid. Otherwise it raises a [YException] *)
  val of_string_opt : ?is_absolute:bool -> string -> t option
  (** [of_string_opt s] returns [Some p] if [s] is a valid path. Otherwise it raises a [YException] *)
  val to_string : t -> string
  (** [to_string path] returns the [path] as a string. *)
  val compare : t -> t-> int
end [@@deriving show]

module Selector : sig
  type t          
  val of_string : ?is_absolute:bool -> string -> t
  val of_string_opt : ?is_absolute:bool -> string -> t option
  val to_string : t -> string
  val get_path : t -> string
  (** [path s] returns the path part of the Selector [s]. I.e. the part before any '?' character. *)
  val get_query : t -> string option
  (** [query s] returns the query part of the Selector [s].
      I.e. the part after the first '?' character and before the fist '#' character, or an empty string if no '?' is found. *)
  val get_fragment : t -> string option
  (** [fragment s] returns the fragment part of the Selector [s].
      I.e. the part after the first '#', or an empty string if no '#' is found. *)
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
