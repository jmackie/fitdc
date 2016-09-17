has_subfields <- function(field_defs) {
  vapply(field_defs, is_dynamic, logical(1))
}

is_dynamic <- function(field_def) {
  # so as to be compatible with vapply
  if (is.null(field_def$field$is_dynamic)) FALSE else TRUE
}

# Create a new list of field definitions updated with the
# appropriate subfield definitions.
resolve_subfields <- function(field_defs, dynamic_i, parsed_field_data) {
  new_field_defs <- field_defs     # returned ultimately.

  for (i in dynamic_i) {
    matching_subfield <- match_subfield(field_defs[[i]], parsed_field_data)
    if (!length(matching_subfield)) next    # recieved `character(0)`
    new_field_defs[[i]]$field <- field_defs[[i]]$field$subfields[[matching_subfield]]
  }

  new_field_defs
}

match_subfield <- function(field_def, parsed_field_data) {
  subfields        <- field_def$field$subfields   # for brevity
  ref_field_names  <- lapply(subfields, `[[`, "ref_field_name")
  ref_field_values <- lapply(subfields, `[[`, "ref_field_value")
  conditions       <- mapply(make_condition, ref_field_names, ref_field_values,
                             SIMPLIFY = FALSE)

  matching_subfield <- vapply(conditions, eval_subfield_condition, logical(1),
                              parsed_field_data, USE.NAMES = FALSE)

  names(conditions)[matching_subfield]  # could be `character(0)`
}

make_condition <- function(field_names, field_values) {
  out <- paste0(field_names, "==", inquotes(field_values), collapse = "||")
  attr(out, "field_names") <- field_names  # for checks prior to eval call
  out
}

eval_subfield_condition <- function(cond, parsed_field_data) {
  if (any(attr(cond, "field_names") %notin% names(parsed_field_data))) {
    FALSE
  } else {
    eval(parse(text = cond), envir = parsed_field_data)
  }
}
