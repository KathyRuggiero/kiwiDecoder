#' Extract key metadata (owner, GPS, original date) from image files
#'
#' This function reads essential EXIF metadata from image files, optionally
#' taking input from a \code{list_image_files()} table. In addition to
#' retrieving GPS coordinates and the original capture date, it also adds a
#' \code{file_owner} field using an internal file‑owner resolver.
#'
#' Fields returned:
#' \itemize{
#'   \item \code{file_owner} — simplified username (no DOMAIN prefix)
#'   \item \code{GPSLatitude}
#'   \item \code{GPSLongitude}
#'   \item \code{DateTimeOriginal}
#' }
#'
#' @param x Character vector of image paths, or a data frame with column
#'   \code{full_path}.
#' @param verbose Print progress messages?
#'
#' @return A list with:
#' \describe{
#'   \item{data}{A tibble containing selected metadata for each file.}
#'   \item{field_summary}{A coverage summary for the selected fields.}
#' }
#'
#' @importFrom exifr read_exif
#' @export
extract_image_metadata <- function(x, verbose = TRUE) {
  
  # ----- Normalize input ---------------------------------------
  if (is.character(x)) {
    files <- x
  } else if (is.data.frame(x) && "full_path" %in% names(x)) {
    files <- x$full_path
  } else {
    stop("Input must be a character vector or a data frame with full_path column.")
  }
  
  files <- trimws(files)
  exists <- file.exists(files)
  
  if (!any(exists)) stop("None of the supplied file paths exist.")
  if (verbose && !all(exists)) {
    warning("Some paths do not exist and will be skipped.")
  }
  
  files <- files[exists]
  
  # ----- Read EXIF ---------------------------------------------
  if (verbose) message("Reading EXIF metadata from ", length(files), " files...")
  
  meta <- exifr::read_exif(files)
  
  if (!nrow(meta)) {
    return(list(data = tibble::tibble(), field_summary = tibble::tibble()))
  }
  
  # ----- Owner --------------------------------------------------
  if (verbose) message("Extracting file owners...")
  owners <- .get_file_owner(files)
  
  # Align by SourceFile
  meta$file_owner <- owners[ match(meta$SourceFile, files) ]
  
  # ----- Select only the required fields ------------------------
  keep_fields <- c(
    "file_owner",
    "GPSLatitude",
    "GPSLongitude",
    "DateTimeOriginal"
  )
  
  missing <- setdiff(keep_fields, names(meta))
  if (length(missing)) meta[missing] <- NA
  
  data_out <- meta[keep_fields]
  
  # ----- Summaries ---------------------------------------------
  if (verbose) message("Summarising field coverage...")
  
  field_summary <- data.frame(
    field = keep_fields,
    n_present = vapply(data_out, function(z) sum(!is.na(z)), integer(1)),
    total_files = nrow(data_out),
    prop_present = vapply(data_out,
                          function(z) mean(!is.na(z)),
                          numeric(1))
  )
  
  list(
    data = data_out,
    field_summary = field_summary
  )
}
