# A mutable object "constructor"
# ==============================

FitFile <- function(conn) {
  header <- NA        # set by `read_fit_file_header`

  bytes_left <- NA    # used to break the reading loop

  update_bytes_left <- function(.bytes_read) {
    # NOTE: we're expecting a negative argument, hence addition.
    bytes_left <<- bytes_left + .bytes_read
  }

  # Definition messages etc
  # -----------------------
  def_mesgs <- list(mesgs = NULL, nums = NULL)
  def_mesg_counter <- 1  # rather than keep calling `length()`

  append_def_mesg <- function(def_mesg) {
    def_mesgs$mesgs[[def_mesg_counter]] <<- def_mesg
    def_mesgs$nums[[def_mesg_counter]]  <<- def_mesg$local_mesg_num

    def_mesg_counter <<- def_mesg_counter + 1   # increment

    def_mesg
  }

  fetch_def_mesg <- function(num) {
    def_mesgs$mesgs[[  # will return NULL by default
      match(num, def_mesgs$nums, nomatch = NA_integer_)]]
  }

  # Data messages etc
  # -----------------
  data_mesgs <- list()
  data_mesg_counter <- 1

  append_data_mesg <- function(data_mesg) {
    data_mesgs[[data_mesg_counter]] <<- data_mesg

    data_mesg_counter <<- data_mesg_counter + 1   # increment

    data_mesg
  }

  # Return this environment, which is passed by reference and mutable!
  environment()
}
