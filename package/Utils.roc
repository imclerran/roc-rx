module [is_digit]

is_digit : U8 -> Bool
is_digit = |c| c >= '0' and c <= '9'