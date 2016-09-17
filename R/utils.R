# Some bitwise operators.
# CRAN check complains if we assign these functions directly!
`%<<%` <- function(a, b) bitwShiftL(a, b)
`%>>%` <- function(a, b) bitwShiftR(a, b)
`%&%`  <- function(a, b) bitwAnd(a, b)


# More for binary reading.
unsigned_I <- function(raw_vector) {  # fmt = "I"
  # 0:31 because this "I" is four bytes (32 bits).
  sum(2 ^ .subset(0:31, bool(rawToBits(raw_vector))))
}


# Better names for builtins!
string <- as.character
bool   <- as.logical


# One liners.
`%notin%` <- function(x, table) match(x, table, nomatch = 0L) == 0L
zchar     <- function(x) !nzchar(x)  # double negative ennit
`%or%`    <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
rm_nulls  <- function(.ls) .ls[!vapply(.ls, is.null, logical(1))]
inquotes  <- function(x) sprintf("'%s'", x)


# Used when forming data messages.
get_field_name <- function(field_def) field_def$field$name %or% ""

extract_field_names <- function(field_defs) {
  .names <- vapply(field_defs, get_field_name, "")
  ifelse(nchar(.names), .names, "unknown")   # FitCSVTool.jar behaviour
}

get_field_units <- function(field_def) field_def$field$units %or% ""

extract_units <- function(field_defs) {
  vapply(field_defs, get_field_units, "")
}


# Decode protocol and profile versions the same way the SDK does.
decode_protocol_version <- function(protocol_venc) {
  as.double(sprintf("%d.%d",
    protocol_venc %>>% 4, protocol_venc %&% ((1 %<<% 4) - 1)
  ))
}

decode_profile_version <- function(profile_venc) {
  as.double(sprintf("%d.%d",
    trunc(profile_venc / 100), profile_venc %% 100
  ))
}
