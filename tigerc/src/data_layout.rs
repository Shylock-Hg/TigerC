// |8 byte for length| data |
pub const ARRAY_HEADER_SIZE: usize = 8;

// |8 byte for string length| string | null |
// compatible with c-string by null-terminated
// so length of data is string length + 1
pub const STRING_HEADER_SIZE: usize = 8;
