#!/usr/bin/Rscript
stopifnot(basename(getwd()) == "scripts")


library(RJSONIO)
library(purrr)


zchar  <- function(x) !nzchar(x)
filter <- function(x, f) Filter(f, x)
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}


# CHANGE THESE PATHS ON DEPLOYMENT.
.Messages   <- fromJSON("Messages.json")
.Types      <- fromJSON("Types.json")


# Base types
# ----------
FIT_BASE_TYPES <- list(
  list(
    name = "enum",
    identifier = 0x00,
    fmt = "B",
    size = 1,
    parse = function(x) if (length(x) && x != 0xFF) x else NULL
  ),
  # -----------------
  list(
    name = "sint8",
    identifier = 0x01,
    fmt = "b",
    size = 1,
    parse = function(x) if (length(x) && x != 0x7F) x else NULL
  ),
  list(
    name = "uint8",
    identifier = 0x02,
    fmt = "B",
    size = 1,
    parse = function(x) if (length(x) && x != 0xFF) x else NULL
  ),
  # -----------------
  list(
    name = "sint16",
    identifier = 0x83,
    fmt = "h",
    size = 2,
    parse = function(x) if (length(x) && x != 0x7FFF) x else NULL
  ),
  list(
    name = "uint16",
    identifier = 0x84,
    fmt = "H",
    size = 2,
    parse = function(x) if (length(x) && x != 0xFFFF) x else NULL
  ),
  # -----------------
  list(
    name = "sint32",
    identifier = 0x85,
    fmt = "i",
    size = 4,
    parse = function(x) if (length(x) && x != 0x7FFFFFFF) x else NULL
  ),
  list(
    name = "uint32",
    identifier = 0x86,
    fmt = "I",
    size = 4,
    parse = function(x) if (length(x) && x != 0xFFFFFFFF) x else NULL
  ),
  # -----------------
  list(
    name = "string",
    identifier = 0x07,
    fmt = "s",
    size = 1,  # ?
    parse = function(x) if (nzchar(x)) x else NULL
  ),
  # -----------------
  list(
    name = "float32",
    identifier = 0x88,
    fmt = "f",
    size = 4,
    parse = function(x) if (is.na(x)) NULL else x  # ?
  ),
  list(
    name = "float64",
    identifier = 0x89,
    fmt = "d",
    size = 8,
    parse = function(x) if (is.na(x)) NULL else x  # ?
  ),
  # -----------------
  list(
    name = "uint8z",
    identifier = 0x0A,
    fmt = "B",
    size = 1,
    parse = function(x) if (length(x) && x) x else NULL
  ),
  list(
    name = "uint16z",
    identifier = 0x8B,
    fmt = "H",
    size = 2,
    parse = function(x) if (length(x) && x) x else NULL
  ),
  list(
    name = "uint32z",
    identifier = 0x8C,
    fmt = "I",
    size = 4,
    parse = function(x) if (length(x) && x) x else NULL
  ),
  # -----------------
  list(  # base type byte
    name = "byte",
    identifier = 0x0D,
    fmt = "B",
    size = 1,
    parse = function(x) if (all(x == 0xFF)) NULL else x  # ?
  )
)
# Restructure BASE_TYPES to be positionally indexed
len <- max(vapply(FIT_BASE_TYPES, `[[`, numeric(1), "identifier")) + 1
tmp <- lapply(seq_len(len), function(x) NULL)

for (type in FIT_BASE_TYPES) {
  tmp[[type$identifier + 1]] <- type
}
FIT_BASE_TYPES <- tmp
rm(len, tmp, type)  # clean up

class(FIT_BASE_TYPES) <- c("BaseTypes")  # for operator overloading

base_type_names <- lapply(FIT_BASE_TYPES, `[[`, "name")  # useful


# Global message numbers and associated codes
# -------------------------------------------
FIT_GLOBAL_MESG_NUMS <- .Types$mesg_num$values
class(FIT_GLOBAL_MESG_NUMS) <- c("GlobalMesgNums")  # for operator overloading


# Field types
# -----------
FIT_FIELD_TYPES <- map(.Types, function(type) {
  # FIELD_TYPES may not need a base type included?
  base_type <- FIT_BASE_TYPES[[match(type$base_type, base_type_names)]]
  values <- type$values
  out <- c(base_type, values = list(values))

  # Provide a method for retrieving value names from codes (default:
  # `character(0)`), as we *can't* implement an S3 method for a subset of the
  # returned list!
  out$get_value_name <- function(code) {
    values$names[match(code, values$codes, nomatch = 0)]
  }

  out
})
# Make into an env for hashing.
FIT_FIELD_TYPES <- list2env(FIT_FIELD_TYPES, hash = TRUE)

field_type_names <- names(FIT_FIELD_TYPES)  # also useful


# Message types
# -------------
make_mesg_type <- function(mesg, name) {
  # NOTE: The incoming message actually just contains fields.

  mesg_num <- with(FIT_GLOBAL_MESG_NUMS, codes[names == name])
  out <- list(name = name, mesg_num = mesg_num)

  make_field <- function(field, field_name = names(field)) {
    field <- as.list(field)  # check
    field <- filter(field, function(f) length(f) > 1 || nzchar(f))
    field$name <- field_name

    if (field$field_type %in% field_type_names) {
      field$type <- FIT_FIELD_TYPES[[field$name]]
    } else if (field$field_type %in% base_type_names) {
      field$type <- FIT_BASE_TYPES[[match(field$field_type, base_type_names)]]
    } # else field$type <- NULL

    field$field_type <- NULL  # del

    field
  }

  fields <- map2(mesg, names(mesg), make_field)

  # Organise sub fields.
  for (field_name in names(fields)) {
    # is this a subfield?
    if (!is.null(parent_field <- fields[[field_name]]$dynamic_parent)) {
      # init a subfields list if not already present.
      if (is.null(fields[[parent_field]]$subfields)) {
        fields[[parent_field]]$subfields <- list()
      }
      fields[[parent_field]]$subfields[[field_name]] <- fields[[field_name]]
      fields[[field_name]] <- NULL   # del
    }
  }
  rm(field_name)  # clean up

  out$fields <- fields
  class(out$fields) <- "FieldDefs"

  out
}

FIT_MESG_TYPES <- map2(.Messages, names(.Messages), make_mesg_type)
# Make into an env for hashing.
FIT_MESG_TYPES <- list2env(FIT_MESG_TYPES, hash = TRUE)


# Write the data for internal use
# -------------------------------
save(list = c("FIT_BASE_TYPES", "FIT_GLOBAL_MESG_NUMS",
              "FIT_FIELD_TYPES", "FIT_MESG_TYPES"),
     file = "../R/sysdata.rda", compress = "xz")
