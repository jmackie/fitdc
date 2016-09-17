# FIT_BASE_TYPES (class: BaseTypes) is a list of lists, with each element
# describing a type of record. The types themselves are given an integer
# identifier (zero- indexed), the largest of which is 141. For faster lookup,
# the outermost list is of length 141, and types are inserted accordingly (only
# with one-based-indexing). Hence, this method is effectively just a
# zero-indexing hack.
#
# Example:
#   R> FIT_BASE_TYPES[[0]]
`[[.BaseTypes` <- function(x, i) x[][[i + 1]]  # need to unclass first


# FIT_GLOBAL_MESG_NUMS (class: GlobalMesgNums) is a list with two elements:
# message `$names` that positionally correspond to message `$codes`. When
# reading a file we'll be given a code, hence this extraction method will
# return the name corresponding to that code.
#
# Example:
#   R> FIT_GLOBAL_MESG_NUMS[[0]]
#
# Default value is `character(0)`; e.g.
#   R> FIT_GLOBAL_MESG_NUMS[[250]]
`[[.GlobalMesgNums` <- function(gmn, code) {
  gmn$names[match(code, gmn$codes, nomatch = 0)]
}


# FIT_MESG_TYPES[[mesg_type]]$fields (class: FieldDefs) a list of named field
# types, each given a numeric identifier. When reading a field definitions
# from a definition message, the fields are identified via an integer (zero-)
# index. Hence, this method allows the field definition to be extracted based
# on the index retrieved from a file.
#
# Example:
#   R> FIT_MESG_TYPES[["file_id"]]$fields
#
# Default value is NULL; e.g.
#   R> FIT_MESG_TYPES[["file_id"]]$fields[[10]]
`[[.FieldDefs` <- function(x, i) {
  x <- unclass(x)
  x[[match(i, lapply(x, `[[`, "field_code"), nomatch = NA_integer_)]]
}
