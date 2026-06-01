# scripts/run_metadata_and_resolve_all.R

source("renv/activate.R")    # required so the background job sees your environment
library(kiwiDecoder)
library(readxl)
library(dplyr)
library(writexl)

root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"

index_files_all <- list.files(
  root,
  pattern = "\\.xlsx$",
  full.names = TRUE,
  recursive = TRUE
)

# Keep only valid kiwiDecoder index files:
index_files <- index_files_all[
  tools::file_path_sans_ext(basename(index_files_all)) ==
    basename(dirname(index_files_all))
]

cat("Found", length(index_files), "index files\n")

# 2. Loop through each .xlsx index file
for (index_path in index_files) {

  cat("Processing:", index_path, "\n")

  index_df <- read_xlsx(index_path)

  # construct full_path internally
  index_df <- index_df %>%
    mutate(full_path = file.path(dir, file_name))

  # extract metadata
  meta <- extract_image_metadata(index_df)

  # join metadata back to index
  index_meta_df <- index_df %>%
    left_join(meta$data, by = "full_path")

  # write back to the same file
  writexl::write_xlsx(index_meta_df, index_path)

  # decode (only for this directory, not global)
  resolve_folder_sequence(dirname(index_path))

  cat("Finished:", index_path, "\n")
}

cat("ALL DIRECTORIES PROCESSED.\n")
