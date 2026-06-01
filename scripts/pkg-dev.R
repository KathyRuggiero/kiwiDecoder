library(dplyr)
library(readr)

source("scripts/run_scan.R")


root <- "//kbc-file.pfr.co.nz/files/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"
chk <- readRDS(file.path(root, "scan_checkpoint.rds"))
tail(chk$dirs_scanned, 20)

# Read Log / Checkpoint

# Log (scan_log.csv): human-readable messages.

# Checkpoint (scan_checkpoint.rds): keeps track of which folders have been scanned and the accumulated index; allows the scan to resume if interrupted.

# Log
log_file <- file.path(image_root, "scan_log.csv")
read_csv(log_file) %>% tail(10)

# Log
scan_file <- file.path(image_root, "scan_heartbeat.csv")
read_csv(scan_file) %>% tail(10)

# Checkpoint (RDS)
checkpoint_file <- file.path(image_root, "scan_checkpoint.rds")
checkpoint <- readRDS(checkpoint_file)
names(checkpoint)

file.remove(file.path(image_root, "scan_log.csv"))
file.remove(file.path(image_root, "scan_checkpoint.rds"))
file.remove(file.path(image_root, "scan_heartbeat.csv"))

# Define log_file path exactly as the scan writes it
#image_root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"
log_file <- file.path(image_root, "scan_log.csv")

# Read the log
log_table <- read_csv(log_file)

log_table %>%
  select(subdir, n_chars) %>%
  tail(10)


current_folder <- readRDS("scan_checkpoint.rds")
nfolders_scanned <- length(current_folder$dirs_scanned)
nfolders_scanned
current_folder$dirs_scanned[nfolders_scanned]

image_root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"

read.csv(file.path(image_root, "scan_log.csv"))

library(kiwiDecoder)
library(readxl)
library(dplyr)
library(writexl)

root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"

# Get all directories under root (including root itself)
dirs <- list.dirs(root, recursive = TRUE)

cat("Found", length(dirs), "directories to scan\n")

xlsx_files <- character(0)

for (i in seq_along(dirs)) {
  d <- dirs[i]
  cat("Scanning dir", i, "of", length(dirs), ":", d, "\n")
  flush.console()

  files_here <- list.files(
    d,
    pattern = "[.]xlsx$",
    full.names = TRUE,
    recursive = FALSE,   # we are already recursing via 'dirs'
    ignore.case = TRUE
  )

  if (length(files_here) > 0) {
    cat("  -> Found", length(files_here), "xlsx file(s)\n")
    xlsx_files <- c(xlsx_files, files_here)
  }
}

cat("\nTotal .xlsx files found:", length(xlsx_files), "\n")

index_root1 <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming/1 Stage 2 Base Photos/1. Orchard photos/2021_22/Flower Buds/1_Superceded/Phil Copy"
index_root2 <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming/1 Stage 2 Base Photos/1. Orchard photos/2024_25/Te Puke/Flower_buds"

joined_results <- resolve_folder_sequence(
  path    = index_root2,
  pattern = "\\.xlsx$"   # or more specific if needed
)
joined_df <- joined_results[[1]]

print_rows <- function(df) {
  for (i in seq_len(nrow(df))) {
    cat(sprintf("row%d:\n", i))
    cat(
      paste0(
        names(df), ' = "',
        as.character(df[i, ]),
        '"',
        collapse = ",\n"
      ),
      "\n\n"
    )
  }
}

print_rows(joined_df[1,])

length(joined_results)
names(joined_results)[1:3]



source("scripts/run_resolve_folder_sequence.R")
monitor_resolve_progress(index_root)

repeat {
  monitor_resolve_progress(index_root)
  Sys.sleep(60)
}

#=== step 1. get file names, run in background
source("scripts/run_scan.R")

library(readxl)

index_path <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images/images.xlsx"

index_df <- read_xlsx(index_path)

# Now extract metadata
meta <- extract_image_metadata(index_df)

# now join

index_meta_df <- index_df %>%
  mutate(full_path = file.path(dir, file_name)) %>%
  left_join(meta$data)

# write back to file
writexl::write_xlsx(index_meta_df, index_path)

source("scripts/run_resolve_folder_sequence.R")

#
library(DBI)
library(RPostgres)

# --- 1. Connect to KiwiCloud (Yugabyte/Postgres) ---
yogicon <- dbConnect(
  Postgres(),
  host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
  port     = 5433,
  dbname   = "kup_obs_comp_prod",
  user     = "kup_ro",
  password = "YOUR_PASSWORD_HERE",   # ← update this
  sslmode  = "disable",
  options  = "-c tcp_keepalives_idle=60 -c tcp_keepalives_interval=20 -c tcp_keepalives_count=5"
)

# --- 2. Preview the first 10 biomaterial-wide rows ---
head_biom <- dbGetQuery(
  yogicon,
  "SELECT *
   FROM public.export_biomaterial_wide
   LIMIT 10;"
)
View(head_biom)


head_obs <- dbGetQuery(
  yogicon,
  "SELECT *
   FROM public.observation
   LIMIT 10;"
)
View(head_obs)

# --- 3. Column names ---
biom_cols <- dbGetQuery(
  yogicon,
  "SELECT column_name
   FROM information_schema.columns
   WHERE table_name = 'export_biomaterial_wide'
   ORDER BY ordinal_position;"
)
print(biom_cols)

# --- 4. Column names + types ---
biom_col_types <- dbGetQuery(
  yogicon,
  "SELECT column_name, data_type
   FROM information_schema.columns
   WHERE table_name = 'export_biomaterial_wide'
   ORDER BY ordinal_position;"
)
print(biom_col_types)
