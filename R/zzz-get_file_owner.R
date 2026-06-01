#' Get file owner usernames from file paths (internal helper)
#'
#' This internal helper attempts to determine the file-system owner for each
#' supplied path and returns a simplified "username" string.
#'
#' On **Windows** it calls `dir /Q` once per unique *directory* (not once per
#' file) and parses the owner from the batch output. This is substantially
#' faster than per-file calls on network drives: ~4,400 images spread across
#' ~20 subfolders requires ~20 shell calls instead of ~4,400.
#'
#' On **Unix-like systems** (including typical Posit Workbench setups) it uses
#' [fs::file_info()] and its `user` column.
#'
#' The raw owner string returned by the OS is often of the form
#' `"DOMAIN\\user.name"` or `"MACHINE\\user"`. This function post-processes
#' that string and returns only the final component after the last backslash
#' (e.g. `"user.name"`), so that downstream code can work with a clean
#' username. If no owner can be determined, `NA_character_` is returned for
#' that path.
#'
#' @param path Character vector of file paths.
#'
#' @return A character vector of the same length as \code{path} containing
#'   the owner username for each file, with any leading domain/machine
#'   component removed. Elements for which the owner cannot be determined
#'   are `NA_character_`.
#'
#' @keywords internal
#'
#' @importFrom fs file_info
.get_file_owner <- function(path) {
  path <- as.character(path)
  n    <- length(path)
  if (!n) return(character(0))

  # Post-process raw owner string: strip leading DOMAIN\ or MACHINE\ component
  clean_owner <- function(owner_raw) {
    if (is.na(owner_raw) || !nzchar(trimws(owner_raw))) return(NA_character_)
    parts <- strsplit(owner_raw, "\\\\")[[1]]
    owner <- trimws(parts[length(parts)])
    if (!nzchar(owner)) NA_character_ else owner
  }

  # ---- Non-Windows branch: use fs::file_info()$user ----------------------
  if (Sys.info()[["sysname"]] != "Windows") {
    if (!requireNamespace("fs", quietly = TRUE)) {
      warning(
        "Package 'fs' is required on non-Windows systems for .get_file_owner(); ",
        "returning NA for all paths."
      )
      return(rep(NA_character_, n))
    }
    res      <- rep(NA_character_, n)
    existing <- file.exists(path)
    if (any(existing)) {
      info      <- fs::file_info(path[existing])
      raw_users <- as.character(info$user)
      res[existing] <- vapply(raw_users, clean_owner, FUN.VALUE = character(1))
    }
    return(res)
  }

  # ---- Windows branch: batch dir /Q by directory -------------------------
  # Running dir /Q on a *directory* returns ownership for every file in one
  # shell call.  Grouping by unique parent directory reduces the number of
  # shell() calls from one-per-file to one-per-directory — a ~200x speedup
  # for a typical 4,400-image run across ~20 subfolders on a network drive.

  res        <- rep(NA_character_, n)
  exists_vec <- file.exists(path)
  if (!any(exists_vec)) return(res)

  # Normalise to backslashes so cmd.exe sees valid paths
  path_norm   <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  unique_dirs <- unique(dirname(path_norm[exists_vec]))

  # Regex to parse a file-entry line from `dir /Q` output.
  # Example line:
  #   "29/10/2025  02:40 PM         2,476,242 PFR\mike.currie   filename.JPG"
  # Works for both 12-hour (AM/PM) and 24-hour locale formats.
  file_line_re <- paste0(
    "^\\s*\\d{1,2}/\\d{1,2}/\\d{4}",   # date  (DD/MM/YYYY or MM/DD/YYYY)
    "\\s+\\d{1,2}:\\d{2}",              # time  (H:MM or HH:MM)
    "(?:\\s*[AP]M)?",                   # optional AM/PM (12-hour locales)
    "\\s+[\\d,]+",                      # file size with optional commas
    "\\s+(\\S+)",                       # owner  — captured group 1, no spaces
    "\\s+(.+?)\\s*$"                    # filename — captured group 2, rest of line
  )

  # owner_lookup: named character vector, names = normalised full path
  owner_lookup <- character(0)

  for (d in unique_dirs) {
    cmd <- paste0('dir /Q "', d, '"')

    dir_output <- tryCatch(
      shell(cmd, intern = TRUE),
      warning = function(w) suppressWarnings(
        tryCatch(shell(cmd, intern = TRUE), error = function(e) character(0))
      ),
      error = function(e) character(0)
    )
    if (!length(dir_output)) next

    for (line in dir_output) {
      # Skip directory entries and non-file lines
      if (grepl("<DIR>", line, fixed = TRUE)) next

      m <- regexec(file_line_re, line, perl = TRUE)[[1L]]
      if (m[1L] == -1L) next

      ml        <- attr(m, "match.length")
      owner_raw <- substr(line, m[2L], m[2L] + ml[2L] - 1L)
      filename  <- trimws(substr(line, m[3L], m[3L] + ml[3L] - 1L))

      if (!nzchar(filename)) next

      key              <- paste0(d, "\\", filename)
      owner_lookup[key] <- clean_owner(owner_raw)
    }
  }

  # Map owners back to the original input order
  for (i in seq_len(n)) {
    if (!exists_vec[i]) next
    key <- path_norm[i]
    if (!is.na(key) && nzchar(key) && key %in% names(owner_lookup)) {
      res[i] <- owner_lookup[[key]]
    }
  }

  res
}
