# Package check and loading (nothing is installed automatically).

required_packages <- c(
  "lpirfs",
  "seasonal",
  "x13binary",
  "sandwich",
  "openxlsx",
  "lubridate",
  "tibble",
  "tidyr",
  "dplyr",
  "ggplot2",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
    "Install them with:\n  install.packages(c(",
    paste0('"', missing_packages, '"', collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  for (pkg in required_packages) library(pkg, character.only = TRUE)
)
rm(pkg, missing_packages)
