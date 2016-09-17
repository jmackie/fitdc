library(purrr)
requireNamespace("plyr", quietly = TRUE)

# Comparison logic for each message goes here...
compare_mesgs <- function(sdk_row, pkg, i) {
  sdk <- restructure_row(sdk_row)

  if (sdk$name != pkg$name) {
    err <- sprintf("(mesg: %d) fields have different names", i)
    message(err); return(i)
  }

  # There shouldn't be *more* fields from the SDK tool!
  if (length(sdk$fields) > length(pkg$fields)) {
    extras <- setdiff(names(sdk$fields), names(pkg$fields))
    # Ignoring enhanced fields
    extras <- grep("enhanced", extras, value = TRUE, invert = TRUE)

    if (length(extras)) {
      extras <- paste(extras, collapse = ", ")
      err <- sprintf(
        "(mesg: %d) different numbers of fields, sdk also gave %s",
        i, extras)
      message(err); return(i)
    }

  }

  # Compare fields common to both.
  shared <- names(sdk$fields)[names(sdk$fields) %in% names(pkg$fields)]
  if (!all(mapply(compare_values, sdk$fields[shared], pkg$fields[shared]))) {
    err <- sprintf("(mesg: %d) found different values", i)
    message(err); return(i)
  }

  return(0)
}

# Helpers
# -------
# Format SDK output to be like this package's output.
restructure_row <- function(row) {
  out <- list(name = row$Message)

  row_fields <- row[grepl("Field|Value|Units", names(row))]
  grp <- map_chr(strsplit(names(row_fields), "\\."), tail, 1)
  row_fields <- split(row_fields, grp)
  row_fields <- map(row_fields, setNames, c("field", "value", "units"))
  row_fields <- Filter(function(x) nzchar(x["field"]), row_fields)

  fields <- map(row_fields, ~try_as_numeric(.["value"]))
  names(fields) <- map_chr(row_fields, "field")
  units  <- unname(map_chr(row_fields, "units"))

  out$fields <- fields
  out$units  <- units
  out
}

# This package will convert some of the numeric values given by the SDK to
# their corresponding strings, so don't try to compare these.
compare_values <- function(sdk, pkg) {
  if (!is.numeric(pkg)) TRUE else sdk == pkg
}

try_as_numeric <- function(x) {
  asnum <- suppressWarnings(as.numeric(x))
  if (is.na(asnum)) x else asnum
}

inspect_mesg <- function(i) {
  # Assumes decoded_sdk and decoded_pkg are already defined!
  pkg <- decoded_pkg[[i]]
  sdk <- restructure_row(decoded_sdk[[i]])

  cat("NAMES\n", "-----\n",
      "pkg: ", pkg$name, "\nsdk: ", sdk$name, "\n\n", sep = "")

  fields <- plyr::rbind.fill(data.frame(pkg$fields), data.frame(sdk$fields))
  row.names(fields) <- c("pkg", "sdk")
  print(fields)
}


# Main
# ----
fitfp <- system.file("extdata/example.fit", package = "readfit")
fit_to_csv_tool <- "./tests/FitCSVTool.jar"

csvfile <- tempfile(fileext = ".csv")
cmd <- "java -jar %s -b %s %s"
system(sprintf(cmd, fit_to_csv_tool, fitfp, csvfile))   # decode with SDK

decoded_sdk <- read.csv(csvfile, stringsAsFactors = FALSE)
decoded_sdk <- transpose(decoded_sdk[decoded_sdk$Type == "Data", ])

decoded_pkg <- read_fit(fitfp)

rm(csvfile, cmd)

if (length(decoded_sdk) != length(decoded_pkg)) {
  stop("number of data messages do not match!")
}

counter <- seq_along(decoded_pkg)
exits   <- mapply(compare_mesgs, decoded_sdk, decoded_pkg, counter)
