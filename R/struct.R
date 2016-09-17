#' Read and unpack bytes from a binary file connection
#'
#' @description This function is exported mainly for my own benefit, but maybe
#'   others will find it useful. It is written to bring the python syntax for
#'   binary file reading to R. See the source code of this package for usage
#'   examples.
#'
#'   Note a limitation of this approach to binary file reading is that
#'   \emph{reading} and \emph{unpacking} are inseparable, which can cause
#'   headaches in some cases.
#'
#' @param fmt a format character according to
#'   \href{https://docs.python.org/3.5/library/struct.html}{the python struct
#'   library docs}. The following are currently supported: \code{"xbBhHiIs"}.
#' @param conn a connection returned by \code{\link[base]{file}}.
#' @param endianness string; passed to \code{\link[base]{readBin}}. One of
#'   \code{"big"} or \code{"little"}.
#' @param n integer; the number of records to read. Also passed to
#'   \code{\link[base]{readBin}}. \strong{NOTE:} this argument is ignored if
#'   \code{fmt = "I"}, due to the way unsigned integers have to be hacked
#'   together.
#' @param ... additional arguments to be passed to \code{\link[base]{readBin}}.
#'
#' @return a "scalar" value according to \code{fmt}.
#'
#' @export
unpack <- function(fmt, conn, endianness = "little", n = 1, ...) {
  # NOTE: file connections are passed by reference.

  switch(fmt,
    # pad byte
    "x" = seek.connection(conn, n, origin = "current", rw = "read", ...),

    # integer types
    # -------------
    # signed char
    "b" = readBin(conn, what = integer(), n = n, size = 1,
                  signed = TRUE, endian = endianness, ...),
    # unsigned char
    "B" = readBin(conn, what = integer(), n = n, size = 1,
                  signed = FALSE, endian = endianness, ...),
    # signed short
    "h" = readBin(conn, what = integer(), n = n, size = 2,
                  signed = TRUE, endian = endianness, ...),
    # unsigned short
    "H" = readBin(conn, what = integer(), n = n, size = 2,
                  signed = FALSE, endian = endianness, ...),
    # int
    "i" = readBin(conn, what = integer(), n = n, size = 4,
                  endian = endianness, ...),
    # unsigned int (need to make it read as unsigned!)
    "I" = unsigned_I(readBin(conn, what = raw(), n = 4 , size = 1,
                             endian = endianness, ...)),

    # other types
    # -----------
    # *NEEDS* an n argument.
    "s" = rawToChar(readBin(conn, what = raw(), n = n, size = 1,
                            endian = endianness, ...)),

    # default
    # -------
    stop("Unrecognised format string.")
  )
}
