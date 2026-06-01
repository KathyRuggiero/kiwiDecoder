.onLoad <- function(libname, pkgname) {
  reticulate::use_condaenv("kiwidecoder-py", required = TRUE)
}
