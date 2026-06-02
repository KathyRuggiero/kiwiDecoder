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

for (code in c("M2126", "T19.58-16-30a", "MC079")) {
  cat("\n=== Searching for:", code, "===\n")

  # 1. In biomaterial_name
  r1 <- dbGetQuery(yogicon, paste0(
    "SELECT biomaterialid, type, biomaterial_name, genotype, current_location_name
     FROM export_biomaterial_wide
     WHERE biomaterial_name ILIKE ", DBI::dbQuoteLiteral(yogicon, paste0("%", code, "%")),
    " LIMIT 3"
  ))
  cat("  In biomaterial_name (ILIKE):", nrow(r1), "rows\n")
  if (nrow(r1) > 0) print(r1)

  # 2. In aliases array
  r2 <- dbGetQuery(yogicon, paste0(
    "SELECT biomaterialid, type, biomaterial_name, genotype, current_location_name, aliases
     FROM export_biomaterial_wide
     WHERE ", DBI::dbQuoteLiteral(yogicon, code), " = ANY(aliases)
     LIMIT 3"
  ))
  cat("  In aliases (exact):", nrow(r2), "rows\n")
  if (nrow(r2) > 0) print(r2)

  # 3. In genotype_synonyms (JSON text search)
  r3 <- dbGetQuery(yogicon, paste0(
    "SELECT biomaterialid, type, biomaterial_name, genotype, genotype_synonyms
     FROM export_biomaterial_wide
     WHERE genotype_synonyms::text ILIKE ", DBI::dbQuoteLiteral(yogicon, paste0("%", code, "%")),
    " LIMIT 3"
  ))
  cat("  In genotype_synonyms:", nrow(r3), "rows\n")
  if (nrow(r3) > 0) print(r3)
}

dbDisconnect(yogicon)
