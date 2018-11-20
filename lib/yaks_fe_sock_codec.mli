open Apero
open Yaks_fe_sock_types

val encode_message : message -> IOBuf.t -> (IOBuf.t, Atypes.error) result

val decode_message : IOBuf.t -> (message * IOBuf.t, Atypes.error) result
