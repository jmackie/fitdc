#' Decode a FIT file
#'
#' @param file_path string; path to the FIT file to be read.
#'
#' @return decoded \emph{data} messages from the FIT file.
#'
#' @examples
#' ## An example of generating a table of record messages
#' ## from the file provided with this package:
#'
#' fp <- system.file("extdata/example.fit", package = "fitdc")
#' data_mesgs <- read_fit(fp)
#'
#' ## Filter out the record messages:
#'
#' is_record <- function(mesg) mesg$name == "record"
#' records <- Filter(is_record, data_mesgs)
#'
#' format_record <- function(record) {
#'   out <- record$fields
#'   names(out) <- paste(names(out), record$units, sep = ".")
#'   out
#' }
#'
#' records <- lapply(records, format_record)
#'
#' ## Some records have missing fields:
#'
#' colnames_full <- names(records[[which.max(lengths(records))]])
#' empty <- setNames(
#'   as.list(rep(NA, length(colnames_full))),
#'   colnames_full)
#'
#' merge_lists <- function(ls_part, ls_full) {
#'   extra <- setdiff(names(ls_full), names(ls_part))
#'   append(ls_part, ls_full[extra])[names(ls_full)]  # order as well
#' }
#'
#' records <- lapply(records, merge_lists, empty)
#' records <- data.frame(
#'   do.call(rbind, records))
#'
#' head(records)  # voila
#' @export
read_fit <- function(file_path) {

  conn <- file(file_path, "rb")  # NOTE: connections are passed by reference
  on.exit(close(conn))

  fit_file <- FitFile(conn)

  fit_file$header <- read_fit_file_header(fit_file)
  fit_file$bytes_left <- fit_file$header$data_size

  # There is a 2-byte CRC at the end of every file.
  while (fit_file$bytes_left > 2) {
    read_fit_message(fit_file)  # messages are appended to fit_file *inplace*!
  }

  fit_file$data_mesgs
}
