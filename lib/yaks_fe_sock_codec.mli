open Apero
open Yaks_fe_sock_types

val encode_value : Yaks_types.Value.t -> IOBuf.t -> (IOBuf.t, Atypes.error) result
val decode_value : IOBuf.t -> (Yaks_types.Value.t * IOBuf.t, Atypes.error) result

val encode_message : message -> IOBuf.t -> (IOBuf.t, Atypes.error) result
val decode_message : IOBuf.t -> (message * IOBuf.t, Atypes.error) result

val encode_message_split : message -> IOBuf.t -> ((IOBuf.t * message option), Atypes.error) result
(** [encode_message_split msg buf] encodes [msg] into [buf].
    But if [msg] has a YPathValueList as body and the buffer is too small to contain the full encoded [msg],
    only a part of YPathValueList is encoded as a message within [buf] and the remaining is returned with [buf]
    as Some new message.
    Otherwise, the resuting buffer is returned with None. *)