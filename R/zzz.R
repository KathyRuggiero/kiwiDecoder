.onLoad <- function(libname, pkgname) {
  # Activate the kiwidecoder-py conda environment.
  # If the environment does not exist yet (e.g. on a fresh install), emit a
  # helpful startup message pointing to setup_kiwidecoder_env() rather than
  # stopping with a cryptic reticulate error.
  tryCatch(
    reticulate::use_condaenv("kiwidecoder-py", required = TRUE),
    error = function(e) {
      packageStartupMessage(
        "\nkiwiDecoder: the 'kiwidecoder-py' conda environment was not found.\n",
        "Run the following to create it and install all required Python packages:\n",
        "  setup_kiwidecoder_env()\n"
      )
    }
  )
}
