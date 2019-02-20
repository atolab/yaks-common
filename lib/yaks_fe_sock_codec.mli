open Yaks_fe_sock_types

val encode_value : Yaks_types.Value.t -> Abuf.t -> unit
val decode_value : Abuf.t -> Yaks_types.Value.t

val encode_message : message -> Abuf.t -> unit
val decode_message : Abuf.t -> message

val encode_message_split : message -> Abuf.t -> message option
(** [encode_message_split msg buf] encodes [msg] into [buf].
    But if [msg] has a YPathValueList as body and the buffer is too small to contain the full encoded [msg],
    only a part of YPathValueList is encoded as a message within [buf] and the remaining is returned with [buf]
    as Some new message.
    Otherwise, the resuting buffer is returned with None. *)