#' Get file owner usernames from file paths (internal helper)
#'
#' This internal helper attempts to determine the file-system owner for each
#' supplied path and returns a simplified "username" string.
#'
#' On **Windows** it calls the `dir /Q` command via `shell()` and parses the
#' owner from the output line corresponding to each file. On **Unix-like
#' systems** (including typical Posit Workbench setups) it uses
#' [fs::file_info()] and its `user` column.
#'
#' The raw owner string returned by the operating system is often of the form
#' `"DOMAIN\\user.name"` or `"MACHINE\\user"`. This function post-processes
#' that string and returns only the final component after the last backslash
#' (e.g. `"user.name"`), so that downstream code can work with a clean
#' username. If no owner can be determined, `NA_character_` is returned for
#' that path.
#'
#' Paths that do not exist result in `NA` for that element.
#'
#' This function is intended for internal use (e.g. by
#' \code{extract_image_metadata()}) and is not exported.
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
#' @examples
#' \dontrun{
#'   # Single path
#'   .get_file_owner("C:/path/to/file.jpg")
#'
#'   # Vector of paths
#'   files <- c("C:/path/to/file1.jpg", "C:/path/to/file2.jpg")
#'   owners <- .get_file_owner(files)
#' }
#'
#' @importFrom fs file_info
.get_file_owner <- function(path) {
  path <- as.character(path)
  n <- length(path)
  if (!n) return(character(0))
  
  # Helper to post-process raw owner string:
  # - if it contains backslashes (e.g. "DOMAIN\\user.name") keep only the
  #   part after the last backslash
  # - otherwise return as-is
  clean_owner <- function(owner_raw) {
    if (is.na(owner_raw) || !nzchar(owner_raw)) {
      return(NA_character_)
    }
    # Split on backslash and take the last component
    parts <- strsplit(owner_raw, "\\\\")[[1]]
    owner <- parts[length(parts)]
    owner <- trimws(owner)
    if (!nzchar(owner)) NA_character_ else owner
  }
  
  # Non-Windows branch: use fs::file_info()$user
  if (Sys.info()[["sysname"]] != "Windows") {
    if (!requireNamespace("fs", quietly = TRUE)) {
      warning(
        "Package 'fs' is required on non-Windows systems for .get_file_owner(); ",
        "returning NA for all paths."
      )
      return(rep(NA_character_, n))
    }
    
    res <- rep(NA_character_, n)
    
    existing <- file.exists(path)
    if (any(existing)) {
      info <- fs::file_info(path[existing])
      raw_users <- as.character(info$user)
      res[existing] <- vapply(raw_users, clean_owner, FUN.VALUE = character(1))
    }
    
    return(res)
  }
  
  # Windows branch: use `dir /Q` via cmd.exe
  res <- rep(NA_character_, n)
  
  for (i in seq_len(n)) {
    p <- path[i]
    if (is.na(p) || !nzchar(p) || !file.exists(p)) {
      res[i] <- NA_character_
      next
    }
    
    # Normalise with backslashes so cmd.exe doesn't treat "/" as switches
    p_norm <- normalizePath(p, winslash = "\\", mustWork = FALSE)
    
    cmd <- paste0('dir /Q "', p_norm, '"')
    
    dir_output <- tryCatch(
      shell(cmd, intern = TRUE),
      warning = function(w) {
        # Non-zero exit status: still try to grab whatever output exists
        suppressWarnings(
          tryCatch(shell(cmd, intern = TRUE), error = function(e) character(0))
        )
      },
      error = function(e) {
        character(0)
      }
    )
    
    if (!length(dir_output)) {
      # No usable output
      res[i] <- NA_character_
      next
    }
    
    bn <- basename(p_norm)
    
    # Find the line containing the basename
    line <- dir_output[grepl(bn, dir_output, fixed = TRUE)]
    if (!length(line)) {
      res[i] <- NA_character_
      next
    }
    line <- line[1]
    
    # Example structure (spaces may vary):
    # 01/02/2026  11:05 AM               4 DOMAIN\\User   filename.jpg
    #
    # Owner is the token immediately before the basename.
    # We'll capture that token via a regex.
    esc_bn <- gsub("([][+.^$(){}|-])", "\\\\\\1", bn)  # escape metacharacters
    pattern <- paste0(".*\\s([^ ]+)\\s+", esc_bn, ".*$")
    owner_raw <- sub(pattern, "\\1", line)
    owner_raw <- trimws(owner_raw)
    
    # If the substitution did nothing, we likely failed to match
    if (!nzchar(owner_raw) || identical(owner_raw, line)) {
      res[i] <- NA_character_
      next
    }
    
    res[i] <- clean_owner(owner_raw)
  }
  
  res
}
