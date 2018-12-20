[%%cenum
type message_id = 
| OPEN [@id 0x01]
| CREATE [@id 0x02]
| DELETE [@id 0x03]
| PUT [@id 0xA0]
| PATCH [@id 0xA1]
| GET [@id 0xA2]
| SUB [@id 0xB0]
| UNSUB [@id 0xB1]
| NOTIFY [@id 0xB2]
| EVAL [@id 0xB3]
| OK [@id 0xD0]
| VALUES [@id 0xD1]
| ERROR [@id 0xE0]
[@@uint8_t]]

[%%cenum 
type value_encoding = 
| RAW [@id  0x01]
| STRING [@id  0x02]
| JSON [@id 0x03]
| PROTOBUF [@id 0x04]
| SQL [@id 0x05]
| PROPERTIES [@id 0x06]
| ENCODING_INVALID [@id 0x0]
[@@uint8_t]]

[%%cenum
type message_flags = 
| PROPERTY [@id 0x01]
| STORAGE [@id 0x02]
| ACCESS [@id 0x04]

[@@uint8_t]]

[%%cenum
type error_code = 
| BAD_REQUEST [@id 400]
| FORBIDDEN [@id 403]
| NOT_FOUND [@id 404]
| PRECONDITION_FAILED [@id 412]
| NOT_IMPLEMENTED [@id 501]
| INSUFFICIENT_STORAGE [@id 507]
[@@uint16_t]]