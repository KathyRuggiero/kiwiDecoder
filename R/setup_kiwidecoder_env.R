#' Set Up the kiwidecoder-py Python Environment
#'
#' Creates the `kiwidecoder-py` conda environment (if it does not already
#' exist), upgrades pip, and installs the three Python packages required by
#' `kiwiDecoder`: Pillow, zxing-cpp, and pillow-heif.
#'
#' This function is safe to run more than once — it checks whether the
#' environment already exists before creating it, and pip will skip packages
#' that are already up to date.
#'
#' @section Package naming:
#' The barcode-reading package is installed from PyPI under the name
#' `"zxing-cpp"` (with a hyphen) but is imported inside Python as `zxingcpp`
#' (no hyphen). This is expected and correct — PyPI distribution names and
#' Python module names are not always the same. `kiwiDecoder` handles this
#' transparently; you do not need to do anything differently.
#'
#' @param python_version Character scalar. Python version to use when creating
#'   the environment. Defaults to `"3.11"`. Python 3.10 is supported but 3.11+
#'   is recommended for best long-term compatibility.
#' @param envname Character scalar. Name of the conda environment. Defaults to
#'   `"kiwidecoder-py"`.
#' @param verbose Logical. If `TRUE` (default), prints progress messages and
#'   pip output during installation.
#'
#' @return Invisibly returns `TRUE` if setup completed successfully (all
#'   packages installed and importable), `FALSE` otherwise.
#'
#' @seealso [reticulate::conda_create()], [reticulate::py_install()]
#'
#' @examples
#' \dontrun{
#'   # First-time setup (run once after installing kiwiDecoder):
#'   setup_kiwidecoder_env()
#'
#'   # Use a specific Python version:
#'   setup_kiwidecoder_env(python_version = "3.12")
#' }
#'
#' @export
setup_kiwidecoder_env <- function(python_version = "3.11",
                                   envname        = "kiwidecoder-py",
                                   verbose        = TRUE) {

  if (verbose) message("── kiwiDecoder Python setup ─────────────────────────────────────────────")

  # ---- 1. Check whether conda is available --------------------------------
  existing <- tryCatch(reticulate::conda_list(), error = function(e) NULL)

  if (is.null(existing)) {
    stop(
      "Could not list conda environments — is Miniconda (or Anaconda) installed?\n",
      "Download Miniconda from: https://docs.conda.io/en/latest/miniconda.html\n",
      "After installing, restart R and run setup_kiwidecoder_env() again."
    )
  }

  # ---- 2. Create the conda environment if needed --------------------------
  if (envname %in% existing$name) {
    if (verbose) message("  * '", envname, "' already exists — skipping creation.")
  } else {
    if (verbose) message("  * Creating conda environment '", envname,
                         "' (Python ", python_version, ")...")
    tryCatch(
      reticulate::conda_create(envname = envname, python_version = python_version),
      error = function(e) stop("Failed to create conda environment: ", conditionMessage(e))
    )
    if (verbose) message("    Done.")
  }

  # ---- 3. Locate Python inside the environment ----------------------------
  python <- tryCatch(
    reticulate::conda_python(envname = envname),
    error = function(e) stop("Could not find Python in '", envname, "': ", conditionMessage(e))
  )
  if (verbose) message("  * Python: ", python)

  # ---- 4. Upgrade pip -----------------------------------------------------
  # Old pip versions may fail to find packages like zxing-cpp.
  if (verbose) message("  * Upgrading pip...")
  out_dest <- if (verbose) "" else NULL     # "" = print to console; NULL = discard
  system2(python, c("-m", "pip", "install", "--upgrade", "pip"),
          stdout = out_dest, stderr = out_dest)

  # ---- 5. Install required packages ---------------------------------------
  # Note: the PyPI distribution name is "zxing-cpp" (with hyphen).
  # Inside Python it is imported as "zxingcpp" — this is expected.
  pkgs <- c("pillow", "zxing-cpp", "pillow-heif")
  if (verbose) message("  * Installing: ", paste(pkgs, collapse = ", "), "...")

  status <- system2(
    python,
    c("-m", "pip", "install", pkgs),
    stdout = out_dest,
    stderr = out_dest
  )

  if (!is.null(status) && status != 0L) {
    warning(
      "pip install returned a non-zero exit code (", status, ").\n",
      "Try running the install manually in an Anaconda Prompt:\n",
      "  conda activate ", envname, "\n",
      "  pip install pillow zxing-cpp pillow-heif"
    )
    return(invisible(FALSE))
  }

  # ---- 6. Verify that all three packages are importable -------------------
  if (verbose) message("  * Verifying imports...")

  tmp <- tempfile(fileext = ".py")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    "import PIL.Image",
    "import pillow_heif",
    "import zxingcpp",
    "print('kiwiDecoder Python setup: all packages OK')"
  ), tmp)

  out <- system2(python, tmp, stdout = TRUE, stderr = TRUE)
  ok  <- any(grepl("all packages OK", out, fixed = TRUE))

  if (ok) {
    if (verbose) {
      message(
        "  * All packages verified successfully.\n",
        "── Setup complete ───────────────────────────────────────────────────────\n",
        "Restart R (or run reticulate::use_condaenv('", envname, "', required = TRUE))\n",
        "then load kiwiDecoder normally:\n",
        "  library(kiwiDecoder)"
      )
    }
  } else {
    warning(
      "Packages were installed but the verification import failed.\n",
      "Output from Python:\n",
      paste(out, collapse = "\n"), "\n\n",
      "Make sure '", envname, "' is the active environment:\n",
      "  reticulate::use_condaenv('", envname, "', required = TRUE)"
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}
