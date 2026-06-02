library(DBI)

yogicon <- dbConnect(
  RPostgres::Postgres(),
  host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
  port     = 5433,
  dbname   = "kup_obs_comp_prod",
  user     = "kup_ro",
  password = Sys.getenv("KUP_DB_PASSWORD"),   # store password in .Renviron â€” never hardcode
  sslmode  = "disable",
  options  = "-c tcp_keepalives_idle=60 -c tcp_keepalives_interval=20 -c tcp_keepalives_count=5"
)

# 1. What types of biomaterials are in the table?
cat("=== Counts by type ===\n")
type_counts <- dbGetQuery(yogicon, "
  SELECT type, COUNT(*) AS n
  FROM export_biomaterial_wide
  GROUP BY type
  ORDER BY n DESC
")
print(type_counts)

# 2. Look for C.91606 specifically in biomaterial_name
cat("\n=== Search biomaterial_name = 'C.91606' ===\n")
c91606 <- dbGetQuery(yogicon, "
  SELECT biomaterialid, type, biomaterial_name, cane_genotype, scion_genotype,
         plant_genotype, genotype, current_location_name, current_location_address,
         current_location_site_name, current_location_site_code,
         current_location_block_name, current_location_row_name,
         current_location_bay_name, current_location_position_name,
         cane_scion_id, scion_plant_id, aliases
  FROM export_biomaterial_wide
  WHERE biomaterial_name = 'C.91606'
")
cat("Rows found:", nrow(c91606), "\n")
if (nrow(c91606) > 0) {
  for (col in names(c91606)) {
    cat(sprintf("  %-40s : %s\n", col, paste(c91606[[col]], collapse = " | ")))
  }
}

# 3. If not in biomaterial_name, check aliases (PostgreSQL text array)
cat("\n=== Search aliases array for 'C.91606' ===\n")
alias_search <- dbGetQuery(yogicon, "
  SELECT biomaterialid, type, biomaterial_name, aliases, cane_genotype, scion_genotype,
         genotype, current_location_name, current_location_address, cane_scion_id
  FROM export_biomaterial_wide
  WHERE 'C.91606' = ANY(aliases)
")
cat("Rows found:", nrow(alias_search), "\n")
if (nrow(alias_search) > 0) {
  for (col in names(alias_search)) {
    cat(sprintf("  %-40s : %s\n", col, paste(alias_search[[col]], collapse = " | ")))
  }
}

# 4. Sample a few CANE type rows to understand that structure
cat("\n=== Sample CANE rows ===\n")
cane_sample <- dbGetQuery(yogicon, "
  SELECT biomaterialid, type, biomaterial_name, cane_genotype, scion_genotype,
         plant_genotype, genotype, current_location_name, current_location_address,
         current_location_block_name, current_location_row_name,
         cane_scion_id, scion_plant_id, aliases
  FROM export_biomaterial_wide
  WHERE type = 'CANE'
  LIMIT 5
")
cat("CANE rows found (limit 5):", nrow(cane_sample), "\n")
if (nrow(cane_sample) > 0) {
  for (col in names(cane_sample)) {
    cat(sprintf("  %-40s : %s\n", col, paste(head(cane_sample[[col]], 3), collapse = " | ")))
  }
}

dbDisconnect(yogicon)
cat("\nDone.\n")
