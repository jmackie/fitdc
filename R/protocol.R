# From the FIT SDK release 20.03.00
#
# The file header provides information about the FIT File. The minimum size of
# the file header is 12 bytes including protocol and profile version numbers,
# the amount of data contained in the file and data type signature. The 12 byte
# header is considered legacy, using the 14 byte header is preferred. The header
# size should always be decoded before attempting to interpret a FIT file,
# Dynastream may extend the header as necessary. Computing the CRC is optional
# when using a 14 byte file header, it is permissible to set it to 0x0000.
# Including the CRC in the file header allows the CRC of the file to be computed
# as the file is being written when the amount of data to be contained in the
# file is not known.
read_fit_file_header <- function(fit_file, conn = fit_file$conn) {

  # Look for ASCII ".FIT" in bytes 8 to 12.
  # (confirming this is actually a .fit file)
  header_data <- readBin(conn, what = raw(), n = 12, endian = "little")

  if (rawToChar(tail(header_data, 4)) != ".FIT") {
    stop("Invalid .FIT file header!")
  }
  # Fine, back to the start...
  seek.connection(conn, where = 0, origin = "start", rw = "read")

  out <- list(
    header_size = unpack("B", conn),
    protocol_v  = decode_protocol_version(unpack("B", conn)),
    # Larger fields are explicitly little endian according to SDK.
    profile_v   = decode_profile_version(unpack("H", conn, "little")),
    data_size   = unpack("I", conn, "little"))

  # Skip over the ASCII ".FIT" code...
  seek.connection(conn, 4, origin = "current", rw = "read")

  extra_header <- out$header_size - 12
  if (extra_header) {
    if (extra_header < 2) {
      stop("Irregular header size.")
    }
    # Skim over the extra bytes (ignoring CRC for now).
    seek.connection(conn, extra_header, origin = "current", rw = "read")
  }
  out
}


# From the FIT SDK release 20.03.00
#
# A FIT record consists of two parts: a record header and the record content.
# The record header indicates whether the record content contains a definition
# message, a normal data message or a compressed timestamp data message. The
# record header also has a local message type field that references the local
# message in the data record to its global FIT message.
read_fit_message <- function(fit_file) {

  mesg_header <- read_fit_message_header(fit_file)

  if (mesg_header$is_definition) {
    read_fit_definition_message(fit_file, mesg_header)
  } else {
    read_fit_data_message(fit_file, mesg_header)
  }
}


# From the FIT SDK release 20.03.00
#
# The record header is a one byte bit field. There are actually two types of
# record header: normal header and compressed timestamp header. The header
# type is indicated in the most significant bit (msb) of the record header.
# The normal header identifies whether the record is a definition or data
# message, and identifies the local message type. A compressed timestamp
# header is a special compressed header that may also be used with some
# local data messages to allow a compressed time format.
#
#
# Normal Header Bit Field Description
# -----------------------------------
#
# =====  =============  ========================
# Bit        Value      Description
# =====  =============  ========================
#   7          0        Normal header
#   6        0 or 1     Message type:
#                         1: definition message
#                         2: data message
#   5     0 (default)   Message type specific
#   4          0        Reserved
#  0-3        0-15      Local message type
# =====  =============  ========================
#
#
# Compressed Timestamp Header Description
# ---------------------------------------
# The compressed timestamp header is a special form of record header that
# allows some timestamp information to be placed within the record header,
# rather than within the record content. In applicable use cases, this
# allows data to be recorded without the need of a 4 byte timestamp in every
# data record.
#
# =====  =============  ========================
# Bit        Value      Description
# =====  =============  ========================
#   7          1        Compressed timestamp
#  5-6        0-3       Local message type
#  0-4        0-31      Time offset (seconds)
# =====  =============  ========================
#
# Note this type of record header is used for a data message only.
read_fit_message_header <- function(fit_file) {
  header_byte <- unpack("B", fit_file$conn)

  # A value of 0 in bit 7 indicates that this is a normal header.
  is_normal_header = !(header_byte %&% 0x80)   # bit 7

  if (is_normal_header) {
    file_header <- list(
      is_definition    = bool(header_byte %&% 0x40),   # bit 6
      local_mesg_num   = header_byte %&% 0xF,          # bits 0-3
      timeoffset       = NULL,
      is_normal_header = is_normal_header)
  } else {
    file_header <- list(
      is_definition    = FALSE,
      local_mesg_num   = (header_byte %>>% 5) %&% 0x3,  # bits 5-6
      timeoffset       = header_byte %&% 0x1F,          # bits 0-4
      is_normal_header = is_normal_header)
  }

  fit_file$update_bytes_left(-1)

  file_header
}


# From the FIT SDK release 20.03.00
#
# The definition message is used to create an association between the local
# message type contained in the record header, and a Global Message Number
# (mesg_num) that relates to the global FIT message.
#
#
# Definition Message Contents
# ---------------------------
#
# ======  =======================  =============  ==============================
# Byte    Description                 Length      Value
# (bytes)
# ======  =======================  =============  ==============================
#   0     Reserved                       1         0
#   1     Architecture                   1         0 or 1
#                                                    0: little endian
#                                                    1: big endian
#  2-3    Global message number          2         Unique to each message
#   4     Fields                         1         Number of fields in the data
#                                                  message
#   5     Field definition(s)            3         See table below
# ...                               (per field)
# ======  =======================  =============  ==============================
#
#
# Field Definition Contents
# -------------------------
#
# ======  =================  ===============================================
#  Byte    Name               Description
# ======  =================  ===============================================
#   0     Field definition   Defined in the gloabl FIT profile for the
#         number             specified FIT message.
#   1     Size               Size (in bytes) of the specified FIT message's
#                            field.
#   2     Base type          Base type of the specified FIT message's field.
# ======  =================  ===============================================
#
read_fit_definition_message <- function(fit_file, mesg_header,
                                        conn = fit_file$conn) {
  unpack("x", conn)   # ignore

  architecture <- unpack("B", conn)
  endianness   <- switch(architecture + 1 , "little", "big")  # default: NULL

  # architecture should be 0 or 1.
  if (is.null(endianness)) {
    warning("Invalid message architecture value found.")
    return(list())  #!
  }

  # Replacing formals will create a local copy of the function.
  formals(unpack)$endianness <- endianness

  global_mesg_num <- unpack("H", conn)
  # The following will default to `character(0)`.
  mesg_type <- FIT_GLOBAL_MESG_NUMS[[global_mesg_num]]  # *overloaded*

  num_fields <- unpack("B", conn)
  field_definitions <- rep(list(NULL), num_fields)  # initialise

  # Pull data out for the definitions:
  for (i in seq_along(field_definitions)) {
    field_def <- list(
      num  = unpack("B", conn),
      size = unpack("B", conn),
      base_type = FIT_BASE_TYPES[[unpack("B", conn)]])     # *overloaded*

    field_def$field <- if (length(mesg_type)) {   # could be character(0)
      # The following will default to NULL.
      FIT_MESG_TYPES[[mesg_type]]$fields[[field_def$num]]  # *overloaded*
    }

    # NOTE: field_def$field could be NULL, in which case field_def$field$units
    #       would still be legal, and would also return NULL.
    #       e.g. x <- NULL; x$y$z

    if (field_def$size %% field_def$base_type$size) {
      stop("Invalid field size encountered.")
    }

    field_definitions[[i]] <- field_def  # append
  }

  bytes_read <- 5 + 3 * num_fields
  fit_file$update_bytes_left(-bytes_read)

  def_mesg <- list(
    name = mesg_type,
    header = mesg_header,
    local_mesg_num = mesg_header$local_mesg_num,
    endianness = endianness,
    field_definitions = field_definitions)

  fit_file$append_def_mesg(def_mesg)
}


read_fit_data_message <- function(fit_file, mesg_header,
                                  conn = fit_file$conn) {

  def_mesg   <- fit_file$fetch_def_mesg(mesg_header$local_mesg_num)
  mesg_name  <- if (length(def_mesg$name)) def_mesg$name else "unknown"
  mesg_start <- seek.connection(fit_file$conn)
  field_defs <- def_mesg$field_definitions
  endianness <- def_mesg$endianness

  parsed_field_data <- parse_field_data_values(field_defs, fit_file, endianness)

  # This may seem a really inefficient way of dealing with subfields, but its
  # currently the best solution I have, and won't actually be called for the
  # majority of data messages (being records, which are without dynamic fields).

  if (mesg_name != "record" &&   # short-circuit here
      any(dynamic_fields <- has_subfields(field_defs))) {

    bytes_left <- fit_file$bytes_left   # save

    # Read again if we have dynamic fields:
    seek.connection(fit_file$conn, mesg_start, origin = "start", rw = "read")
    field_defs <- resolve_subfields(field_defs, which(dynamic_fields), parsed_field_data)
    parsed_field_data <- parse_field_data_values(field_defs, fit_file, endianness)

    bytes_left -> fit_file$bytes_left   # restore
  }

  data_mesg <- list(name   = mesg_name,
                    fields = rm_nulls(parsed_field_data),
                    units  = extract_units(field_defs))

  fit_file$append_data_mesg(data_mesg)
}


# Helper functions (that are best kept here)
# ----------------
parse_field_data_values <- function(field_defs, fit_file, endianness) {
  .data <- lapply(field_defs, parse_field_data_value, fit_file, endianness)
  names(.data) <- extract_field_names(field_defs)   # utils.R
  .data
}

parse_field_data_value <- function(field_def, fit_file, endianness) {

  n_to_read <- trunc(field_def$size / field_def$base_type$size)   # check this?
  raw_value <- unpack(field_def$base_type$fmt, fit_file$conn, endianness, n_to_read)

  value <- try_apply_scale_offset(raw_value, field_def$field)

  value_mapping_fun <- field_def$field$type$get_value_name
  if (!is.null(value_mapping_fun)) {
    value <- value_mapping_fun(value)
  }

  bytes_read <- field_def$base_type$size * n_to_read
  fit_file$update_bytes_left(-bytes_read)

  field_def$base_type$parse(value)
}

# # From the FIT SDK release 20.03.00
#
# The FIT SDK supports applying a scale or offset to binary fields. This allows
# efficient representation of values within a particular range and provides a
# convenient method for representing floating point values in integer systems. A
# scale or offset may be specified in the FIT profile for binary fields
# (sint/uint etc.) only. When specified, the binary quantity is divided by the
# scale factor and then the offset is subtracted, yielding a floating point
# quantity.
#
# e.g. FIT_MESG_TYPES$record$fields$altitude
try_apply_scale_offset <- function(raw_value, field) {
  if (is.numeric(raw_value)) {   # could be a string
    raw_value / (field$scale %or% 1) - (field$offset %or% 0)
  } else raw_value
}
