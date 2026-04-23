# R/zzz.R

# Create a hidden environment to store the Python module
# A package-level cache so Python is only initialised once per session.
#' Initialise (once per session) the Python environment with the signing helpers.
#'
#' Imports the required Python modules and defines `create_signature` so it can
#' be called from R via reticulate.  The result is cached in a package-level
#' environment so the import overhead only occurs once.
#'
#' @return A reticulate Python module / namespace containing `create_signature`.
#' @keywords internal
.kalshi_py_env <- new.env(parent = emptyenv())

#' Initialise (once per session) the Python environment with the signing helpers.
#'
#' Imports the required Python modules and defines `create_signature` so it can
#' be called from R via reticulate.  The result is cached in `.kalshi_py_env`
#' so the import overhead only occurs once.
#'
#' @return A reticulate Python namespace containing `create_signature`.
#' @keywords internal
.get_kalshi_py_env <- function() {

  # Return cached environment if already initialised
  if (!is.null(.kalshi_py_env$py)) {
    return(.kalshi_py_env$py)
  }

  # Ensure reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for RSA signing. ",
         "Install it with: install.packages('reticulate')",
         call. = FALSE)
  }

  # Import required Python modules
  py_env <- reticulate::py_run_string("
import base64
import datetime
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding

def create_signature(private_key, timestamp, method, path):
    path_without_query = path.split('?')[0]
    message = (str(timestamp) + method + path_without_query).encode('utf-8')
    signature = private_key.sign(
        message,
        padding.PSS(
            mgf=padding.MGF1(hashes.SHA256()),
            salt_length=padding.PSS.DIGEST_LENGTH
        ),
        hashes.SHA256()
    )
    return base64.b64encode(signature).decode('utf-8')
")

  # Cache and return
  .kalshi_py_env$py <- py_env
  .kalshi_py_env$py
}

.onLoad <- function(libname, pkgname) {
  # 1. Check for/Install dependencies from your DESCRIPTION file
  reticulate::configure_environment(pkgname)

  # 2. Find the path
  path <- system.file("python/kalshi_functions.py", package = pkgname)

  if (file.exists(path)) {
    # 3. Use delay_load = TRUE so R doesn't crash if Python is missing
    # until you actually try to use a function.
    mod <- reticulate::import_from_path(
      "kalshi_functions",
      path = dirname(path),
      delay_load = TRUE
    )

    # Store it in our hidden environment
    assign("kalshi_py", mod, envir = .kalshi_py_env)
  }
}
