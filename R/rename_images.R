
# Packages
# install.packages(c("readxl", "dplyr", "stringr", "fs")) # run once if needed
library(readxl)
library(dplyr)
library(stringr)
library(fs)
library(tools)   # for file_ext(), file_path_sans_ext()

# ====== 1) Inputs ======
base_dir   <- "N:/Projects/Stage2 Clonal Trials/Images/A Named photos/2 Stage 2 Male photos/Male Photos 2024_25"          # <-- directory where the files live
excel_path <- file.path(base_dir, "decoded_results mike updated.xlsx")  # <-- put your Excel path here
sheet      <- NULL  # or "Sheet1" if you need a specific sheet

# Safety toggles
dry_run        <- FALSE  # TRUE = preview only, FALSE = actually rename
create_backups <- TRUE   # TRUE = copy originals to a backup folder before renaming

# Optional backup folder (if create_backups = TRUE)
backup_dir <- file.path(base_dir, "_backup_before_rename")
if (create_backups && !dir_exists(backup_dir)) dir_create(backup_dir)

# ====== 2) Helper: sanitise text for filenames ======
sanitize_for_filename <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("[\\/:*?\"<>|]", "") %>%   # remove illegal filename chars
    str_replace_all("\\s+", "_") %>%           # spaces -> underscores
    str_replace_all("_+", "_")                 # collapse multiple underscores
}

# ====== 3) Read Excel ======
# Keeps original column names (including spaces). If sheet is NULL, reads the first sheet.
df <- read_excel(excel_path, sheet = sheet)

# Make sure the required columns exist
required_cols <- c("original_filename", "Actual Genotype", "Actual location_name")
missing <- setdiff(required_cols, names(df))
if (length(missing) > 0) stop(paste("Missing columns in Excel:", paste(missing, collapse = ", ")))

# ====== 4) Build new filenames ======
out <- df %>%
  mutate(
    # If original_filename is just a name, use base_dir; if it already has a path, keep it
    original_basename = basename(original_filename),
    original_dir      = ifelse(original_filename == original_basename,
                               base_dir,
                               dirname(original_filename)),
    
    ext      = file_ext(original_basename),
    stem     = file_path_sans_ext(original_basename),
    
    # Sanitise components
    stem_s      = sanitize_for_filename(stem),
    genotype_s  = sanitize_for_filename(`Actual Genotype`),
    location_s  = sanitize_for_filename(`Actual location_name`),
    
    # Compose new basename, keep "." before extension
    new_basename = ifelse(ext == "",
                          paste0(stem_s, "_", genotype_s, "_", location_s),
                          paste0(stem_s, " ", genotype_s, " ", location_s, ".", ext)),
    
    old_path = path(original_dir, original_basename),
    new_path = path(original_dir, new_basename)
  )

# Resolve duplicates by making names unique (adds _1, _2, … if needed)
out <- out %>%
  group_by(original_dir) %>%
  mutate(new_path = {
    # make.unique works on strings; we want to apply it to the basename within each directory
    base_names <- basename(new_path)
    unique_basenames <- make.unique(base_names, sep = "_")
    path(original_dir, unique_basenames)
  }) %>%
  ungroup()

# ====== 5) Preview and optionally rename ======
# Check existence and conflicts
out <- out %>%
  mutate(
    old_exists = file_exists(old_path),
    new_conflicts = file_exists(new_path)
  )

# Report what will happen
preview <- out %>%
  transmute(
    old_path,
    new_path,
    will_rename = old_exists & !new_conflicts
  )

print(preview)

View(preview)

# Optional backup of originals
if (create_backups) {
  to_backup <- out %>% filter(old_exists)
  invisible(
    purrr::pwalk(
      list(to_backup$old_path,
           path(backup_dir, basename(to_backup$old_path))),
      ~ file_copy(..1, ..2, overwrite = TRUE)
    )
  )
}

# Actual rename (if dry_run == FALSE)
if (!dry_run) {
  out <- out %>%
    mutate(
      rename_ok = mapply(function(src, dest) {
        if (!file_exists(src)) {
          message("Missing source: ", src)
          return(FALSE)
        }
        if (file_exists(dest)) {
          message("Target already exists, skipping: ", dest)
          return(FALSE)
        }
        file_move(src, dest)  # rename within same directory
        TRUE
      }, old_path, new_path)
    )
} else {
  out <- out %>% mutate(rename_ok = NA)
  message("Dry-run complete. Set dry_run <- FALSE to perform the rename.")
}

# ====== 6) Write an audit CSV ======
audit_path <- path(base_dir, paste0("rename_audit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
write.csv(
  out %>% select(original_filename, `Actual Genotype`, `Actual location_name`, old_path, new_path, rename_ok),
  audit_path,
  row.names = FALSE
)
message("Audit written to: ", audit_path)
