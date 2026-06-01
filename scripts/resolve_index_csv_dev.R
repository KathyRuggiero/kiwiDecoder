#' Resolve and Decode Barcodes for a Single CSV Index
#' (Symbol-Level, Stateful Propagation, One-to-Many Join)
#'
#' @param index_csv_path Path to a single CSV index file
#'        (as produced by scan_directories() / your CSV writer).
#'
#' @return Invisibly returns the enriched index tibble.
#' @export
resolve_index_csv <- function(index_csv_path) {

  if (!file.exists(index_csv_path)) {
    stop("Index CSV does not exist: ", index_csv_path)
  }

  message("Processing index CSV: ", index_csv_path)

  df_orig <- readr::read_csv(index_csv_path, show_col_types = FALSE)

  if (!"file_name" %in% names(df_orig)) {
    stop("Index missing required column 'file_name': ", index_csv_path)
  }

  # Ensure full_path and dir are available
  if (!"full_path" %in% names(df_orig)) {
    if (!all(c("dir", "file_name") %in% names(df_orig))) {
      stop("Index must contain either 'full_path' or 'dir' + 'file_name'")
    }
    df_orig$full_path <- file.path(df_orig$dir, df_orig$file_name)
  }

  if (!"dir" %in% names(df_orig)) {
    df_orig$dir <- dirname(df_orig$full_path)
  }
  if (!"subdir" %in% names(df_orig)) {
    df_orig$subdir <- basename(df_orig$dir)
  }
  if (!"rel_path" %in% names(df_orig)) {
    df_orig$rel_path <- NA_character_
  }

  # -------------------------------------------------------------------------
  # Logging: one row per image
  # -------------------------------------------------------------------------
  log_file <- file.path(dirname(index_csv_path), "resolve_log.csv")

  # -------------------------------------------------------------------------
  # Filename prefix-number parser: "IMG_0001.JPG" -> prefix="IMG", number=1
  # -------------------------------------------------------------------------
  parse_sequence <- function(fname) {
    stem  <- tools::file_path_sans_ext(basename(fname))
    m     <- regexec("^([A-Za-z]+)_([0-9]+)$", stem)
    parts <- regmatches(stem, m)[[1]]
    if (length(parts) != 3L) {
      return(list(prefix = NA_character_, number = NA_integer_))
    }
    list(prefix = parts[2], number = as.integer(parts[3]))
  }

  n_images <- nrow(df_orig)
  message("  Decoding ", n_images, " image(s)...")

  pb      <- utils::txtProgressBar(min = 0, max = n_images, style = 3)
  decoded <- vector("list", n_images)

  # -------------------------------------------------------------------------
  # Decode each image → symbol-level long rows
  # -------------------------------------------------------------------------
  for (j in seq_len(n_images)) {

    fname     <- df_orig$file_name[j]
    full_path <- df_orig$full_path[j]

    decoded_tbl <- tryCatch(
      decode_hierarchical_zxing(full_path),
      error = function(e) {
        message("\n    ! Error decoding ", fname, ": ", e$message)
        tibble::tibble(
          index       = NA_integer_,
          code_format = NA_character_,
          text        = NA_character_,
          type        = NA_character_
        )
      }
    )

    # Per-image logging
    seq_parsed   <- parse_sequence(fname)
    decoded_flag <- any(!is.na(decoded_tbl$text) & decoded_tbl$text != "")

    log_entry <- data.frame(
      time       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      index_csv  = index_csv_path,
      image_file = fname,
      full_path  = full_path,
      prefix     = seq_parsed$prefix,
      number     = seq_parsed$number,
      decoded    = decoded_flag,
      stringsAsFactors = FALSE
    )

    utils::write.table(
      log_entry,
      file      = log_file,
      append    = TRUE,
      sep       = ",",
      col.names = !file.exists(log_file),
      row.names = FALSE
    )

    # Attach context columns (for propagation later)
    decoded_tbl$file_name <- fname
    decoded_tbl$full_path <- full_path
    decoded_tbl$rel_path  <- df_orig$rel_path[j]
    decoded_tbl$dir       <- df_orig$dir[j]
    decoded_tbl$subdir    <- df_orig$subdir[j]

    decoded[[j]] <- decoded_tbl
    utils::setTxtProgressBar(pb, j)
  }

  close(pb)

  long_df <- dplyr::bind_rows(decoded)

  # -------------------------------------------------------------------------
  # Build image-level sequencing table
  # -------------------------------------------------------------------------
  image_df <- df_orig
  seq_info <- lapply(image_df$file_name, parse_sequence)
  image_df$prefix <- vapply(seq_info, `[[`, "prefix", FUN.VALUE = character(1))
  image_df$number <- vapply(seq_info, `[[`, "number", FUN.VALUE = integer(1))

  dec_summary <- long_df |>
    dplyr::group_by(file_name) |>
    dplyr::summarise(
      has_symbol = any(!is.na(text) & text != ""),
      .groups    = "drop"
    )

  image_df <- dplyr::left_join(image_df, dec_summary, by = "file_name")
  image_df$has_symbol[is.na(image_df$has_symbol)] <- FALSE

  image_df <- dplyr::arrange(image_df, prefix, number)

  # -------------------------------------------------------------------------
  # Stateful donor tracking (same logic as resolve_folder_sequence)
  # -------------------------------------------------------------------------
  current_donor  <- NA_character_
  current_prefix <- NA_character_
  last_number    <- NA_integer_

  image_df$donor      <- NA_character_
  image_df$break_case <- FALSE

  for (r in seq_len(nrow(image_df))) {
    fn  <- image_df$file_name[r]
    px  <- image_df$prefix[r]
    num <- image_df$number[r]
    has <- image_df$has_symbol[r]

    if (has) {
      current_donor          <- fn
      current_prefix         <- px
      last_number            <- num
      image_df$donor[r]      <- fn
      image_df$break_case[r] <- FALSE

    } else if (!is.na(current_donor) &&
               px == current_prefix &&
               !is.na(num) && num == last_number + 1) {

      image_df$donor[r]      <- current_donor
      image_df$break_case[r] <- FALSE
      last_number            <- num

    } else {
      current_donor          <- NA_character_
      current_prefix         <- px
      last_number            <- num
      image_df$donor[r]      <- NA_character_
      image_df$break_case[r] <- TRUE
    }
  }

  # -------------------------------------------------------------------------
  # Propagate full symbol sets
  # -------------------------------------------------------------------------
  propagated <- vector("list", nrow(image_df))

  for (r in seq_len(nrow(image_df))) {

    img   <- image_df$file_name[r]
    donor <- image_df$donor[r]

    if (!is.na(donor)) {
      donor_rows <- long_df[
        long_df$file_name == donor &
          !is.na(long_df$text) & long_df$text != "",
        ,
        drop = FALSE
      ]

      if (nrow(donor_rows) == 0L) {
        df_img <- tibble::tibble(
          file_name   = img,
          index       = NA_integer_,
          code_format = NA_character_,
          text        = NA_character_,
          type        = NA_character_
        )
      } else {
        df_img <- donor_rows
        df_img$file_name <- img
        df_img$full_path <- file.path(image_df$dir[r], img)
        df_img$dir       <- image_df$dir[r]
        df_img$subdir    <- image_df$subdir[r]
        df_img$rel_path  <- image_df$rel_path[r]
      }

    } else {
      this_rows <- long_df[long_df$file_name == img, , drop = FALSE]

      if (nrow(this_rows) == 0L) {
        df_img <- tibble::tibble(
          file_name   = img,
          index       = NA_integer_,
          code_format = NA_character_,
          text        = NA_character_,
          type        = NA_character_
        )
      } else {
        df_img <- this_rows
      }

      if (isTRUE(image_df$break_case[r])) {
        no_symbols <- all(
          (is.na(df_img$code_format) | df_img$code_format == "") &
            (is.na(df_img$text)        | df_img$text        == "") &
            (is.na(df_img$type)        | df_img$type        == "")
        )
        if (no_symbols) {
          df_img$code_format <- "unknown"
          df_img$text        <- "unknown"
          df_img$type        <- "unknown"
        }
      }
    }

    df_img$prefix <- image_df$prefix[r]
    df_img$number <- image_df$number[r]

    propagated[[r]] <- df_img
  }

  long_df_prop <- dplyr::bind_rows(propagated)

  # -------------------------------------------------------------------------
  # Join back to index (1-to-many)
  # -------------------------------------------------------------------------
  join_cols <- intersect(
    c("file_name","full_path","index","code_format","text","type","prefix","number"),
    names(long_df_prop)
  )

  barcodes <- long_df_prop[, join_cols, drop = FALSE]

  joined_df <- df_orig |>
    dplyr::left_join(barcodes, by = "file_name")

  message("  Writing updated CSV...")
  readr::write_csv(joined_df, index_csv_path)

  invisible(joined_df)
}
