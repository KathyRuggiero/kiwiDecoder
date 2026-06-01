#' Extract EXIF metadata (owner, GPS, capture date) for image files
#'
#' `extract_image_metadata()` reads selected EXIF metadata for a set of image
#' files. It is designed to be used immediately after [scan_directories()],
#' which produces per-directory index tables containing `dir` and `file_name`
#' columns, or it can be applied to a character vector of full image paths.
#'
#' The function automatically determines how to interpret its input:
#'
#' \itemize{
#'   \item If `x` is a **character vector**, it is treated as full paths.
#'   \item If `x` is a **data frame with a `full_path` column**, it is used
#'         directly.
#'   \item If `x` is a **data frame with both `dir` and `file_name`
#'         columns**, then `full_path` is constructed internally using
#'         `file.path(dir, file_name)`.
#' }
#'
#' For each existing file, the following fields are extracted:
#'
#' \itemize{
#'   \item \code{full_path}         – the absolute file path,
#'   \item \code{file_owner}        – simplified username (no DOMAIN prefix),
#'   \item \code{camera_make}       – camera manufacturer (e.g. "Apple"),
#'   \item \code{camera_model}      – camera model (e.g. "iPhone 13"),
#'   \item \code{GPSLatitude}       – GPS latitude (if present),
#'   \item \code{GPSLongitude}      – GPS longitude (if present),
#'   \item \code{DateTimeOriginal}  – original capture date/time (if present).
#' }
#'
#' Additional details:
#' \itemize{
#'   \item Missing or non-existent paths are skipped with a warning.
#'   \item EXIF metadata is read in a single batch using [exifr::read_exif()].
#'   \item Output rows are reordered to match the input file order exactly.
#'   \item One row is returned per file, and `full_path` is always included
#'         in the returned data for easy joining.
#'   \item A coverage summary (`field_summary`) is produced for all returned
#'         metadata fields.
#' }
#'
#' @param x Either:
#'   \itemize{
#'     \item a character vector of image paths,
#'     \item a data frame with a `full_path` column,
#'     \item or a data frame containing both `dir` and `file_name` columns.
#'   }
#' @param verbose Logical; print progress messages?
#'
#' @return A list with two components:
#'   \describe{
#'     \item{data}{A tibble with one row per file containing:
#'       \code{full_path}, \code{file_owner}, \code{camera_make},
#'       \code{camera_model}, \code{GPSLatitude},
#'       \code{GPSLongitude}, \code{DateTimeOriginal}.}
#'
#'     \item{field_summary}{A tibble with one row per field summarising:
#'       \code{field}, number of non-missing entries (\code{n_present}),
#'       total files, and proportion present (\code{prop_present}).}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Using index table from scan_directories()
#'   idx <- readxl::read_xlsx("folder/my_index.xlsx")
#'   meta <- extract_image_metadata(idx)
#'   dplyr::left_join(idx, meta$data, by = "full_path")
#'
#'   # Using a vector of paths
#'   files <- c("IMG_0001.JPG", "IMG_0002.JPG")
#'   meta2 <- extract_image_metadata(files)
#' }
#'
#' @importFrom exifr read_exif
#' @importFrom tibble tibble as_tibble
#' @export
extract_image_metadata <- function(x, verbose = TRUE) {

  # ----- Normalise input ----------------------------------------------------
  if (is.character(x)) {

    # x = character vector of paths
    files <- x

  } else if (is.data.frame(x)) {

    if ("full_path" %in% names(x)) {

      # Already has full_path (from earlier pipeline)
      files <- x$full_path

    } else if (all(c("dir", "file_name") %in% names(x))) {

      # Construct full_path from dir + file_name (the scan_directories format)
      files <- file.path(x$dir, x$file_name)

    } else {

      stop(
        "Data frame must contain either 'full_path' or both 'dir' and 'file_name'."
      )
    }

  } else {
    stop("Input must be a character vector or a data frame.")
  }

  files  <- trimws(files)
  exists <- file.exists(files)

  if (!any(exists)) {
    stop("None of the supplied file paths exist.")
  }

  if (verbose && !all(exists)) {
    missing <- files[!exists]
    warning(
      "Some paths do not exist and will be skipped:\n",
      paste(missing, collapse = "\n")
    )
  }

  # Keep only existing files (still in original order)
  files <- files[exists]

  # ----- Read EXIF in batch -------------------------------------------------
  if (verbose) {
    message("Reading EXIF metadata from ", length(files), " file(s)...")
  }

  meta <- exifr::read_exif(files)

  # If nothing is returned, bail out early with empty structures
  if (!nrow(meta)) {
    if (verbose) message("No EXIF metadata returned.")
    empty <- tibble::tibble()
    return(list(
      data          = empty,
      field_summary = empty
    ))
  }

  # Ensure EXIF rows are in the same order as input 'files'
  if ("SourceFile" %in% names(meta)) {
    meta <- meta[match(files, meta$SourceFile), , drop = FALSE]
  }

  # ----- File owner using internal helper ----------------------------------
  if (verbose) message("Extracting file owners...")
  owners <- .get_file_owner(files)  # must return a vector same length as 'files'

  if ("SourceFile" %in% names(meta)) {
    meta$file_owner <- owners[match(meta$SourceFile, files)]
  } else {
    # Fallback: assume meta rows correspond to files in order
    meta$file_owner <- owners
  }

  # ----- Select only the required fields -----------------------------------
  keep_fields <- c(
    "file_owner",
    "Make",
    "Model",
    "GPSLatitude",
    "GPSLongitude",
    "DateTimeOriginal"
  )

  # Ensure missing columns exist
  missing <- setdiff(keep_fields, names(meta))
  if (length(missing)) {
    for (fld in missing) {
      meta[[fld]] <- NA
    }
  }

  # Build output table: one row per file, with full_path as join key
  data_out <- tibble::tibble(
    full_path        = files,
    file_owner       = meta$file_owner,
    camera_make      = meta$Make,
    camera_model     = meta$Model,
    GPSLatitude      = meta$GPSLatitude,
    GPSLongitude     = meta$GPSLongitude,
    DateTimeOriginal = meta$DateTimeOriginal
  )

  # ----- Summaries ----------------------------------------------------------
  if (verbose) message("Summarising field coverage...")

  field_names <- names(data_out)

  field_summary <- data.frame(
    field        = field_names,
    n_present    = vapply(data_out, function(z) sum(!is.na(z)), integer(1)),
    total_files  = nrow(data_out),
    prop_present = vapply(
      data_out,
      function(z) if (nrow(data_out) > 0L) mean(!is.na(z)) else NA_real_,
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
  list(
    data          = tibble::as_tibble(data_out),
    field_summary = tibble::as_tibble(field_summary)
  )
}
