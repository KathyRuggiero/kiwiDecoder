#' Parse Sequential Image Identifiers from Filenames
#'
#' `parse_sequence_id()` extracts a **character prefix** and a **numeric sequence
#' number** from an image filename. It is designed for filenames such as
#' `"IMG_0001.jpg"`, `"PMC_0123.png"`, or `"P1234567.jpeg"`.
#'
#' The function is **pure**:
#' it performs no I/O, has no side effects, and does not depend on external state.
#'
#' @param filename Character scalar. Image filename (with or without path).
#'
#' @return A list with components:
#' \describe{
#'   \item{prefix}{Character scalar. Alphabetic prefix identifying the sequence
#'   (e.g. `"IMG"`, `"PMC"`, `"DSC"`).}
#'   \item{number}{Integer. Parsed sequence number.}
#' }
#'
#' @details
#' Filenames must match the pattern:
#'
#' \preformatted{
#'   PREFIX_NUMBER.EXT
#' }
#'
#' where:
#' * `PREFIX` is one or more letters
#' * `NUMBER` is one or more digits
#'
#' If the filename does not conform, an error is raised.
#'
#' @examples
#' parse_sequence_id("IMG_0001.jpg")
#' parse_sequence_id("PMC_0123.png")
#'
#' @export
parse_sequence_id <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(filename))
  
  m <- regexec("^([A-Za-z]+)_?([0-9]+)$", stem)
  parts <- regmatches(stem, m)[[1]]
  
  if (length(parts) != 3) {
    stop("Filename does not match expected PREFIX_NUMBER format: ", filename)
  }
  
  list(
    prefix = parts[2],
    number = as.integer(parts[3])
  )
}
