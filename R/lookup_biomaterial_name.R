#' Look up biomaterial names and location from decoded barcode text
#'
#' Given one or more decoded barcode text strings (as returned by
#' [decode_hierarchical_zxing()] or [detect_codes_all_zxing()]), queries the
#' `export_biomaterial_wide` table in Yugabyte and returns the matching scion
#' biomaterial name, genotype, and location.
#'
#' Three barcode types are handled:
#'
#' \describe{
#'   \item{Cane codes (`C.` prefix)}{The decoded text is the cane's
#'     `biomaterial_name`. A single self-JOIN retrieves the connected scion's
#'     name via `cane_scion_id`.}
#'   \item{Scion codes (`SC-` prefix)}{The decoded text is already the scion's
#'     `biomaterial_name` and is queried directly.}
#'   \item{Other codes}{Searched in `biomaterial_name` across all types. Codes
#'     not found in the table return `NA` for all lookup columns.}
#' }
#'
#' Batched parameterised queries are used so the function is efficient for
#' large decoded result sets (one query per barcode type, not one per row).
#'
#' @param codes Character vector of decoded barcode text strings. `NA` and
#'   empty strings are silently dropped before querying.
#' @param con A DBI connection object pointing to the Yugabyte database
#'   (`kup_obs_comp_prod`). Create with:
#'   \preformatted{
#'   con <- DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     host     = "...",
#'     port     = 5433,
#'     dbname   = "kup_obs_comp_prod",
#'     user     = "kup_ro",
#'     password = "...",
#'     sslmode  = "disable"
#'   )
#'   }
#'
#' @return A tibble with one row per unique, non-missing code in `codes`:
#' \describe{
#'   \item{`code`}{The input decoded text.}
#'   \item{`input_type`}{`"cane"`, `"scion"`, or `"other"`.}
#'   \item{`cane_name`}{Cane `biomaterial_name`, or `NA` if input is not a cane.}
#'   \item{`scion_name`}{Scion `biomaterial_name` (the unique KiwiCloud name for
#'     the grafted shoot).}
#'   \item{`genotype`}{Genotype name (e.g., `"K17.0004.03072"`).}
#'   \item{`location_name`}{`current_location_name` from the table.}
#'   \item{`location_address`}{`current_location_address` — structured address
#'     such as `"TRC-17-13-19c"`.}
#'   \item{`site_name`}{`current_location_site_name`.}
#'   \item{`block`}{`current_location_block_name`.}
#'   \item{`row`}{`current_location_row_name`.}
#'   \item{`bay`}{`current_location_bay_name`.}
#'   \item{`position`}{`current_location_position_name`.}
#'   \item{`found`}{Logical — `TRUE` if the code was matched in the database.}
#' }
#'
#' @seealso [decode_hierarchical_zxing()], [resolve_folder_sequence()]
#'
#' @importFrom DBI dbGetQuery
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
lookup_biomaterial_name <- function(codes, con) {

  if (!inherits(con, "DBIConnection")) {
    stop("`con` must be a DBI connection object.")
  }

  # Drop NA / empty
  codes <- unique(codes[!is.na(codes) & nzchar(codes)])
  if (length(codes) == 0L) {
    return(.empty_lookup_tibble())
  }

  # Classify each code by prefix
  is_cane  <- startsWith(codes, "C.")
  is_scion <- startsWith(codes, "SC-")
  is_other <- !is_cane & !is_scion

  results <- list()

  # ------------------------------------------------------------------
  # Cane codes — one self-JOIN query for all cane codes at once
  # ------------------------------------------------------------------
  if (any(is_cane)) {
    cane_codes <- codes[is_cane]
    rows <- DBI::dbGetQuery(con, paste0("
      SELECT
        cane.biomaterial_name                 AS code,
        cane.biomaterial_name                 AS cane_name,
        scion.biomaterial_name                AS scion_name,
        cane.genotype                         AS scion_genotype,
        cane.current_location_name            AS location_name,
        cane.current_location_address         AS location_address,
        cane.current_location_site_name       AS site_name,
        cane.current_location_block_name      AS block,
        cane.current_location_row_name        AS row,
        cane.current_location_bay_name        AS bay,
        cane.current_location_position_name   AS position
      FROM export_biomaterial_wide AS cane
      LEFT JOIN export_biomaterial_wide AS scion
        ON scion.biomaterialid = cane.cane_scion_id
      WHERE cane.biomaterial_name IN (", .in_list(con, cane_codes), ")
        AND cane.type = 'CANE'
    "))

    rows$input_type <- "cane"
    rows$found      <- TRUE
    results[["cane"]] <- rows

    # Codes not found
    missing <- setdiff(cane_codes, rows$code)
    if (length(missing) > 0L) {
      results[["cane_missing"]] <- .na_rows(missing, "cane")
    }
  }

  # ------------------------------------------------------------------
  # Scion codes — direct lookup
  # ------------------------------------------------------------------
  if (any(is_scion)) {
    scion_codes <- codes[is_scion]
    rows <- DBI::dbGetQuery(con, paste0("
      SELECT
        s.biomaterial_name                    AS code,
        NULL::text                            AS cane_name,
        s.biomaterial_name                    AS scion_name,
        s.genotype                            AS scion_genotype,
        s.current_location_name               AS location_name,
        s.current_location_address            AS location_address,
        s.current_location_site_name          AS site_name,
        s.current_location_block_name         AS block,
        s.current_location_row_name           AS row,
        s.current_location_bay_name           AS bay,
        s.current_location_position_name      AS position
      FROM export_biomaterial_wide AS s
      WHERE s.biomaterial_name IN (", .in_list(con, scion_codes), ")
        AND s.type = 'SCION'
    "))

    rows$input_type <- "scion"
    rows$found      <- TRUE
    results[["scion"]] <- rows

    missing <- setdiff(scion_codes, rows$code)
    if (length(missing) > 0L) {
      results[["scion_missing"]] <- .na_rows(missing, "scion")
    }
  }

  # ------------------------------------------------------------------
  # Other codes — try biomaterial_name first, then aliases array.
  # Clonal IDs (e.g. M2126, MC079) live in genotype_synonyms JSON and
  # intentionally map to many plants; they are left as not-found here.
  # ------------------------------------------------------------------
  if (any(is_other)) {
    other_codes <- codes[is_other]

    # Pass 1: exact biomaterial_name match
    rows1 <- DBI::dbGetQuery(con, paste0("
      SELECT
        b.biomaterial_name                    AS code,
        NULL::text                            AS cane_name,
        b.biomaterial_name                    AS scion_name,
        b.genotype                            AS scion_genotype,
        b.current_location_name               AS location_name,
        b.current_location_address            AS location_address,
        b.current_location_site_name          AS site_name,
        b.current_location_block_name         AS block,
        b.current_location_row_name           AS row,
        b.current_location_bay_name           AS bay,
        b.current_location_position_name      AS position
      FROM export_biomaterial_wide AS b
      WHERE b.biomaterial_name IN (", .in_list(con, other_codes), ")
    "))

    found1  <- rows1$code
    missing1 <- setdiff(other_codes, found1)

    # Pass 2: alias array match for codes not found in biomaterial_name
    rows2 <- if (length(missing1) > 0L) {
      DBI::dbGetQuery(con, paste0("
        SELECT
          matched_alias                           AS code,
          NULL::text                              AS cane_name,
          b.biomaterial_name                      AS scion_name,
          b.genotype                              AS scion_genotype,
          b.current_location_name                 AS location_name,
          b.current_location_address              AS location_address,
          b.current_location_site_name            AS site_name,
          b.current_location_block_name           AS block,
          b.current_location_row_name             AS row,
          b.current_location_bay_name             AS bay,
          b.current_location_position_name        AS position
        FROM export_biomaterial_wide AS b,
             unnest(b.aliases) AS matched_alias
        WHERE matched_alias IN (", .in_list(con, missing1), ")
      "))
    } else {
      data.frame()
    }

    found2   <- if (nrow(rows2) > 0L) rows2$code else character()
    missing2 <- setdiff(missing1, found2)

    all_rows <- dplyr::bind_rows(rows1, rows2)
    all_rows$input_type <- "other"
    all_rows$found      <- TRUE
    if (nrow(all_rows) > 0L) results[["other"]] <- all_rows

    if (length(missing2) > 0L) {
      results[["other_missing"]] <- .na_rows(missing2, "other")
    }
  }

  out <- dplyr::bind_rows(results)

  # Ensure canonical column order
  col_order <- c("code", "input_type", "found", "cane_name", "scion_name",
                 "scion_genotype", "location_name", "location_address", "site_name",
                 "block", "row", "bay", "position")
  out[, intersect(col_order, names(out)), drop = FALSE]
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.empty_lookup_tibble <- function() {
  tibble::tibble(
    code             = character(),
    input_type       = character(),
    found            = logical(),
    cane_name        = character(),
    scion_name       = character(),
    scion_genotype   = character(),
    location_name    = character(),
    location_address = character(),
    site_name        = character(),
    block            = character(),
    row              = character(),
    bay              = character(),
    position         = character()
  )
}

# Build a safe SQL IN (...) list from a character vector using dbQuoteLiteral.
# This avoids any SQL-injection risk without needing parameterised arrays.
.in_list <- function(con, values) {
  paste(DBI::dbQuoteLiteral(con, values), collapse = ", ")
}

.na_rows <- function(codes, input_type) {
  tibble::tibble(
    code             = codes,
    input_type       = input_type,
    found            = FALSE,
    cane_name        = NA_character_,
    scion_name       = NA_character_,
    scion_genotype   = NA_character_,
    location_name    = NA_character_,
    location_address = NA_character_,
    site_name        = NA_character_,
    block            = NA_character_,
    row              = NA_character_,
    bay              = NA_character_,
    position         = NA_character_
  )
}
