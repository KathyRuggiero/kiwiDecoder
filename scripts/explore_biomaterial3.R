library(DBI)

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

cane_scion_uuid <- "e2a0b1fc-76c8-4f00-b3ce-f3c8726481eb"

# 1. Look up the SCION row linked from C.91606
cat("=== SCION row linked from C.91606 (via cane_scion_id) ===\n")
scion_row <- dbGetQuery(yogicon, "
  SELECT biomaterialid, type, biomaterial_name, cane_genotype, scion_genotype,
         plant_genotype, genotype, current_location_name, current_location_address,
         current_location_site_name, current_location_block_name,
         current_location_row_name, current_location_bay_name,
         current_location_position_name, cane_scion_id, scion_plant_id, aliases,
         genotype_synonyms
  FROM export_biomaterial_wide
  WHERE biomaterialid = $1
", params = list(cane_scion_uuid))
cat("Rows found:", nrow(scion_row), "\n")
for (col in names(scion_row)) {
  cat(sprintf("  %-40s : %s\n", col, paste(scion_row[[col]], collapse = " | ")))
}

# 2. Check whether SC-002-6MY appears in biomaterial_name (from the label)
cat("\n=== Search for 'SC-002-6MY' in biomaterial_name ===\n")
sc_row <- dbGetQuery(yogicon, "
  SELECT biomaterialid, type, biomaterial_name, scion_genotype, plant_genotype,
         genotype, current_location_name, current_location_address,
         cane_scion_id, scion_plant_id, aliases, genotype_synonyms
  FROM export_biomaterial_wide
  WHERE biomaterial_name = 'SC-002-6MY'
     OR 'SC-002-6MY' = ANY(aliases)
")
cat("Rows found:", nrow(sc_row), "\n")
if (nrow(sc_row) > 0) {
  for (col in names(sc_row)) {
    cat(sprintf("  %-40s : %s\n", col, paste(sc_row[[col]], collapse = " | ")))
  }
}

# 3. What does a SCION type row look like generally?
cat("\n=== Sample 3 SCION type rows ===\n")
scion_sample <- dbGetQuery(yogicon, "
  SELECT biomaterialid, type, biomaterial_name, cane_genotype, scion_genotype,
         plant_genotype, genotype, current_location_name, current_location_address,
         cane_scion_id, scion_plant_id, aliases
  FROM export_biomaterial_wide
  WHERE type = 'SCION'
    AND biomaterial_name IS NOT NULL
  LIMIT 3
")
for (col in names(scion_sample)) {
  cat(sprintf("  %-40s : %s\n", col, paste(scion_sample[[col]], collapse = " | ")))
}

# 4. Single-query JOIN approach: cane + its scion in one hit
cat("\n=== JOIN: C.91606 cane + its linked scion (single query) ===\n")
joined <- dbGetQuery(yogicon, "
  SELECT
    cane.biomaterial_name                AS cane_name,
    cane.genotype                        AS genotype,
    cane.current_location_name           AS location_name,
    cane.current_location_address        AS location_address,
    cane.current_location_site_name      AS site_name,
    cane.current_location_block_name     AS block,
    cane.current_location_row_name       AS row,
    cane.current_location_bay_name       AS bay,
    cane.current_location_position_name  AS position,
    scion.biomaterial_name               AS scion_name,
    scion.type                           AS scion_type,
    scion.aliases                        AS scion_aliases
  FROM export_biomaterial_wide AS cane
  LEFT JOIN export_biomaterial_wide AS scion
    ON scion.biomaterialid = cane.cane_scion_id
  WHERE cane.biomaterial_name = 'C.91606'
")
cat("Rows:", nrow(joined), "\n")
for (col in names(joined)) {
  cat(sprintf("  %-40s : %s\n", col, paste(joined[[col]], collapse = " | ")))
}

dbDisconnect(yogicon)
cat("\nDone.\n")
