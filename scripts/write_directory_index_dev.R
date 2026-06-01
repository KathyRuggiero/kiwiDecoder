#' Write Directory Index (Fast CSV)
#'
#' Generates an index of image files in a directory, optionally filtered by
#' extensions and filename prefixes. Writes a CSV file.
#'
#' @param dir Character. Directory to scan.
#' @param extensions Character vector of file extensions (case-insensitive).
#' @param prefixes Character vector of regex for filename stems; NULL disables filtering.
#' @param csv_path Optional path to CSV file. Default: "_index.csv" in same folder.
#' @param log_file Optional log file to append messages.
#'
#' @return Invisibly: data.frame of indexed files.
#' @importFrom fs dir_ls path_file path_ext file_info
#' @importFrom utils write.csv
#' @importFrom stringr str_detect
#' @export

write_directory_index <- function(
    dir,
    extensions = c("jpg", "jpeg", "png"),
    prefixes   = c("^PMC_[0-9]{5}$", "^DSC_[0-9]{4}$",
                   "^IMG_[0-9]{4}$", "^P[0-9]{7}$"),
    csv_path   = NULL,
    log_file   = NULL
) {
  if (!dir.exists(dir)) stop("Directory does not exist: ", dir)

  subdir   <- basename(dir)
  csv_path <- csv_path %||% file.path(dir, paste0(subdir, "_index.csv"))

  log_msg <- function(...) {
    if (!is.null(log_file)) {
      msg <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste(..., collapse = " "))
      writeLines(msg, file = log_file, append = TRUE)
    }
  }

  all_files <- fs::dir_ls(dir, type="file", recurse=FALSE)
  files <- all_files[stringr::str_detect(all_files, paste0("(?i)\\.(", paste(extensions, collapse="|"), ")$"))]

  if (!is.null(prefixes) && length(prefixes) > 0 && length(files) > 0) {
    stems <- tools::file_path_sans_ext(fs::path_file(files))
    match_mask <- Reduce(`|`, lapply(prefixes, function(px) stringr::str_detect(stems, px)))
    files <- files[match_mask]
  }

  if (length(files) == 0) {
    log_msg("No matching image files in", dir)
    return(data.frame())
  }

  file_info <- fs::file_info(files)
  index <- data.frame(
    file_name = fs::path_file(files),
    file_stem = tools::file_path_sans_ext(fs::path_file(files)),
    file_ext  = fs::path_ext(files),
    full_path = files,
    file_size = file_info$size,
    mod_time  = file_info$modification_time,
    dir       = dir,
    stringsAsFactors=FALSE
  )

  utils::write.csv(index, csv_path, row.names=FALSE)
  log_msg("Indexed", length(files), "files in", dir)

  invisible(index)
}
