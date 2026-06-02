pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
  port     = 5433,
  dbname   = "kup_obs_comp_prod",
  user     = "kup_ro",
  password = Sys.getenv("KUP_DB_PASSWORD"),   # store password in .Renviron â€” never hardcode
  sslmode  = "disable",
  options  = "-c tcp_keepalives_idle=60 -c tcp_keepalives_interval=20 -c tcp_keepalives_count=5"
)

# Codes decoded from our test images
test_codes <- c(
  "C.91606",       # cane â€” from IMG_2774 (fixed by Stage 5 upscale)
  "C.91632",       # cane â€” from IMG_2570 (fixed by Stage 2 halves)
  "C.91640",       # cane â€” from IMG_2573 (always worked)
  "M2126",         # from IMG_0127 (unknown type â€” general lookup)
  "SC-002-2QQ",    # scion â€” from IMG_0127
  "T19.58-16-30a", # from IMG_1024 (unknown type â€” general lookup)
  "RS022354",      # from IMG_6688
  "MC079",         # from IMG_6688
  "SC-002-JWM",    # scion â€” from IMG_6688
  "DOESNOTEXIST"   # should return found = FALSE
)

cat("Running lookup_biomaterial_name() on decoded test codes...\n\n")
result <- lookup_biomaterial_name(test_codes, con)

print(as.data.frame(result), right = FALSE)

dbDisconnect(con)
