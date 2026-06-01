library(DBI)
library(dplyr)

yogicon <- dbConnect(
  RPostgres::Postgres(),
  host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
  port     = 5433,
  dbname   = "kup_obs_comp_prod",
  user     = "kup_ro",
  password = Sys.getenv("YUGABYTE_PW"),   # store password in .Renviron — never hardcode
  sslmode  = "disable",
  options  = "-c tcp_keepalives_idle=60 -c tcp_keepalives_interval=20 -c tcp_keepalives_count=5"
)

cat("=== Column names in export_biomaterial_wide ===\n")
cols <- dbListFields(yogicon, "export_biomaterial_wide")
cat(paste(cols, collapse = "\n"), "\n\n")

cat("=== 3 sample rows (all columns) ===\n")
sample_rows <- dbGetQuery(yogicon, "SELECT * FROM export_biomaterial_wide LIMIT 3")
for (col in names(sample_rows)) {
  cat(sprintf("  %-40s : %s\n", col, paste(sample_rows[[col]], collapse = " | ")))
}

cat("\n=== Look for C.9xxxx pattern — rows where any text column matches ===\n")
# Try to find where C.91606 or similar codes appear
search_result <- dbGetQuery(yogicon, "
  SELECT *
  FROM export_biomaterial_wide
  WHERE name ILIKE 'C.9%'
     OR name ILIKE 'C.8%'
  LIMIT 10
")
cat("Rows matching name ILIKE 'C.9%' or 'C.8%':", nrow(search_result), "\n")
if (nrow(search_result) > 0) {
  for (col in names(search_result)) {
    cat(sprintf("  %-40s : %s\n", col, paste(head(search_result[[col]], 3), collapse = " | ")))
  }
}

cat("\n=== Try biomaterial_name or barcode or code columns ===\n")
# Check for columns that might hold the decoded barcode value
barcode_cols <- grep("name|code|barcode|label|text|scion|cane|id", cols, value = TRUE, ignore.case = TRUE)
cat("Candidate columns:", paste(barcode_cols, collapse = ", "), "\n")

cat("\n=== Search across likely text columns for 'C.91606' ===\n")
# Build a query that searches across all character-type columns
col_types <- dbGetQuery(yogicon, "
  SELECT column_name, data_type
  FROM information_schema.columns
  WHERE table_name = 'export_biomaterial_wide'
    AND table_schema = 'public'
  ORDER BY ordinal_position
")
cat("Column types:\n")
for (i in seq_len(nrow(col_types))) {
  cat(sprintf("  %-40s : %s\n", col_types$column_name[i], col_types$data_type[i]))
}

dbDisconnect(yogicon)
cat("\nDone.\n")
