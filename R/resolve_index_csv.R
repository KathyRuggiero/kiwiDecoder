#' Resolve and Decode Barcodes for a Single CSV Index File
#'
#' `resolve_index_csv()` is the single-file counterpart to
#' [resolve_folder_sequence()]. It reads one CSV index file (as produced by
#' [scan_directories()] / [write_directory_index()]), decodes all barcodes and
#' QR codes, applies stateful symbol-level propagation across sequentially
#' numbered filenames, and writes the enriched result back to the same CSV.
#'
#' This function is useful for processing a single folder interactively or for
#' re-running a specific index without touching the rest of the tree.
#'
#' Parallel decoding is used automatically when a \pkg{future} parallel plan
#' has been set before calling this function (e.g.
#' \code{future::plan(future::multisession, workers = 4)}). Falls back to
#' sequential \code{lapply()} if \pkg{furrr} is not installed.
#'
#' Log entries are written in a single batch at the end of processing (not
#' one-per-image), to minimise file I/O on network drives.
#'
#' @param index_csv_path Path to a single CSV index file produced by
#'   [scan_directories()] or [write_directory_index()].
#' @param con Optional DBI connection to Yugabyte. When supplied, decoded codes
#'   are passed to [lookup_biomaterial_name()] and the returned fields
#'   (`scion_name`, `genotype`, `location_name`, `location_address`,
#'   `site_name`, `block`, `row`, `bay`, `position`) are appended to the
#'   output. Pass \code{NULL} (the default) to skip the lookup.
#'
#' @return Invisibly returns the enriched tibble, which is also written back
#'   to \code{index_csv_path}.
#'
#' @seealso [resolve_folder_sequence()] for batch processing of all CSV index
#'   files under a root directory, [scan_directories()] for generating the
#'   index files.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr bind_rows group_by summarise arrange left_join
#' @importFrom tibble tibble
#' @importFrom utils write.table
#' @export
resolve_index_csv <- function(index_csv_path, con = NULL, overwrite = FALSE) {

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
  if (!"dir" %in% names(df_orig)) df_orig$dir <- dirname(df_orig$full_path)

  log_file <- file.path(dirname(index_csv_path), "resolve_log.csv")

  parse_sequence <- function(fname) {
    stem  <- tools::file_path_sans_ext(basename(fname))
    m     <- regexec("^([A-Za-z]+)_([0-9]+)$", stem)
    parts <- regmatches(stem, m)[[1]]
    if (length(parts) != 3L) return(list(prefix = NA_character_, number = NA_integer_))
    list(prefix = parts[2], number = as.integer(parts[3]))
  }

  # ---------------------------------------------------------------------------
  # Incremental mode: skip images already decoded in a previous run.
  # ---------------------------------------------------------------------------
  previously_resolved <- !overwrite && "code_format" %in% names(df_orig)

  if (previously_resolved) {
    already_done <- unique(df_orig$file_name[!is.na(df_orig$code_format)])
    new_names    <- setdiff(unique(df_orig$file_name), already_done)

    if (length(new_names) == 0L) {
      message("  All images already decoded — skipping (use overwrite = TRUE to re-run).")
      return(invisible(df_orig))
    }

    message("  ", length(new_names), " new image(s) to decode (",
            length(already_done), " already processed).")

    df_to_decode <- df_orig[df_orig$file_name %in% new_names, , drop = FALSE]
    df_to_decode <- df_to_decode[!duplicated(df_to_decode$file_name), , drop = FALSE]

    old_cols <- intersect(
      c("file_name", "full_path", "dir", "index", "code_format", "text", "type"),
      names(df_orig)
    )
    old_long <- df_orig[
      !is.na(df_orig$code_format) & df_orig$code_format != "unknown",
      old_cols, drop = FALSE
    ]

  } else {
    already_done <- character(0)
    new_names    <- unique(df_orig$file_name)
    df_to_decode <- df_orig[!duplicated(df_orig$file_name), , drop = FALSE]
    old_long     <- NULL
  }

  n_images <- nrow(df_to_decode)
  message("  Decoding ", n_images, " image(s)...")

  # ---------------------------------------------------------------------------
  # Decode each image → symbol-level long rows
  # Parallel if furrr is available and a future::plan() has been set.
  # ---------------------------------------------------------------------------
  decode_one <- function(j) {
    fname     <- df_to_decode$file_name[j]
    full_path <- df_to_decode$full_path[j]

    decoded_tbl <- tryCatch(
      decode_hierarchical_zxing(full_path),
      error = function(e) {
        tibble::tibble(
          index       = NA_integer_,
          code_format = NA_character_,
          text        = NA_character_,
          type        = NA_character_
        )
      }
    )

    seq_parsed   <- parse_sequence(fname)
    decoded_flag <- any(!is.na(decoded_tbl$text) & decoded_tbl$text != "")

    decoded_tbl$file_name <- fname
    decoded_tbl$full_path <- full_path
    decoded_tbl$dir       <- df_to_decode$dir[j]

    list(
      decoded_tbl = decoded_tbl,
      log_entry   = data.frame(
        time          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        csv_file      = index_csv_path,
        image_file    = fname,
        full_path     = full_path,
        prefix        = seq_parsed$prefix,
        number        = seq_parsed$number,
        decoded       = decoded_flag,
        stringsAsFactors = FALSE
      )
    )
  }

  decode_results <- if (requireNamespace("furrr", quietly = TRUE)) {
    furrr::future_map(
      seq_len(n_images),
      decode_one,
      .options = furrr::furrr_options(packages = "kiwiDecoder")
    )
  } else {
    lapply(seq_len(n_images), decode_one)
  }

  decoded   <- lapply(decode_results, `[[`, "decoded_tbl")
  log_batch <- dplyr::bind_rows(lapply(decode_results, `[[`, "log_entry"))

  # Write entire batch at once (one file open/close, not one per image)
  write.table(
    log_batch,
    file      = log_file,
    append    = TRUE,
    sep       = ",",
    col.names = !file.exists(log_file),
    row.names = FALSE
  )

  long_df_new <- dplyr::bind_rows(decoded)

  long_df <- if (!is.null(old_long) && nrow(old_long) > 0L) {
    dplyr::bind_rows(old_long, long_df_new)
  } else {
    long_df_new
  }

  # ---------------------------------------------------------------------------
  # Build image-level sequencing table using full unique file list
  # ---------------------------------------------------------------------------
  img_unique_cols <- intersect(
    c("dir", "file_name", "ext"),
    names(df_orig)
  )
  image_df         <- df_orig[!duplicated(df_orig$file_name), img_unique_cols, drop = FALSE]
  seq_info         <- lapply(image_df$file_name, parse_sequence)
  image_df$prefix  <- vapply(seq_info, `[[`, "prefix", FUN.VALUE = character(1))
  image_df$number  <- vapply(seq_info, `[[`, "number", FUN.VALUE = integer(1))

  dec_summary <- long_df |>
    dplyr::group_by(file_name) |>
    dplyr::summarise(
      has_symbol = any(!is.na(text) & text != ""),
      .groups    = "drop"
    )

  image_df <- dplyr::left_join(image_df, dec_summary, by = "file_name")
  image_df$has_symbol[is.na(image_df$has_symbol)] <- FALSE
  image_df <- dplyr::arrange(image_df, prefix, number)

  # ---------------------------------------------------------------------------
  # Stateful donor tracking
  # ---------------------------------------------------------------------------
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
               !is.na(num) && num == last_number + 1L) {

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

  # ---------------------------------------------------------------------------
  # Propagate full symbol sets
  # ---------------------------------------------------------------------------
  # Two columns record how each row's barcode information was obtained:
  #
  #   decode_status:
  #     "decoded"      – barcode text read directly from this image's barcode
  #     "propagated"   – text borrowed from a sequentially adjacent image that
  #                       did decode; treat as indicative, not confirmed
  #     "undecodable"  – image was processed but no barcode could be decoded
  #
  #   donor_file:
  #     The file_name of the image whose barcode was propagated (only set when
  #     decode_status == "propagated").  Useful for manual review: find the
  #     source image and check whether the code is appropriate for this image.
  # ---------------------------------------------------------------------------
  prop_list <- vector("list", nrow(image_df))

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
          file_name = img, index = NA_integer_,
          code_format = NA_character_, text = NA_character_, type = NA_character_
        )
        df_img$decode_status <- NA_character_
        df_img$donor_file    <- NA_character_
      } else {
        df_img           <- donor_rows
        df_img$file_name <- img
        df_img$full_path <- file.path(image_df$dir[r], img)
        df_img$dir       <- image_df$dir[r]
        if (donor == img) {
          df_img$decode_status <- "decoded"
          df_img$donor_file    <- NA_character_
        } else {
          df_img$decode_status <- "propagated"
          df_img$donor_file    <- donor
        }
      }

    } else {
      this_rows <- long_df[long_df$file_name == img, , drop = FALSE]

      if (nrow(this_rows) == 0L) {
        df_img <- tibble::tibble(
          file_name = img, index = NA_integer_,
          code_format = NA_character_, text = NA_character_, type = NA_character_
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
          # Set code_format = "unknown" as a machine sentinel so that
          # incremental-mode reruns recognise this image as already processed
          # and do not re-decode it.  Leave text and type as NA.
          df_img$code_format <- "unknown"
        }
        df_img$decode_status <- "undecodable"
      } else {
        df_img$decode_status <- "decoded"
      }

      df_img$donor_file <- NA_character_
    }

    prop_list[[r]]  <- df_img
  }

  long_df_prop <- dplyr::bind_rows(prop_list)

  # ---------------------------------------------------------------------------
  # Optional biomaterial lookup (requires a live Yugabyte connection)
  # ---------------------------------------------------------------------------
  if (!is.null(con)) {
    lookup_codes <- unique(long_df_prop$text[
      !is.na(long_df_prop$text) &
        nzchar(long_df_prop$text)
    ])
    if (length(lookup_codes) > 0L) {
      message("  Looking up biomaterial names for ", length(lookup_codes),
              " unique code(s)...")
      lkp <- tryCatch(
        lookup_biomaterial_name(lookup_codes, con),
        error = function(e) {
          warning("lookup_biomaterial_name() failed: ", conditionMessage(e)); NULL
        }
      )
      if (!is.null(lkp) && nrow(lkp) > 0L) {
        lkp_sel <- lkp[, intersect(
          c("code", "scion_name", "scion_genotype", "location_name", "location_address"),
          names(lkp)
        ), drop = FALSE]
        long_df_prop <- dplyr::left_join(long_df_prop, lkp_sel,
                                         by = c("text" = "code"))
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Join back to index (1-to-many) and write
  # ---------------------------------------------------------------------------
  join_cols <- intersect(
    c("file_name", "index", "code_format", "text", "type",
      "decode_status", "donor_file",
      "scion_name", "scion_genotype", "location_name", "location_address"),
    names(long_df_prop)
  )

  if (length(already_done) > 0L) {
    old_enriched <- df_orig[df_orig$file_name %in% already_done, , drop = FALSE]
    new_barcodes <- long_df_prop[long_df_prop$file_name %in% new_names,
                                  join_cols, drop = FALSE]
    new_joined   <- dplyr::left_join(df_to_decode, new_barcodes, by = "file_name")
    joined_df    <- dplyr::bind_rows(old_enriched, new_joined)
  } else {
    joined_df <- df_orig |>
      dplyr::left_join(long_df_prop[, join_cols, drop = FALSE], by = "file_name")
  }

  message("  Writing updated CSV...")
  readr::write_csv(joined_df, index_csv_path)

  invisible(joined_df)
}
