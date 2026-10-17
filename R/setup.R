# Project paths, package loading and sourcing of the function files.

find_project_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "run_all.R")) && dir.exists(file.path(d, "R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) {
      stop("Project root not found: run the code from inside the project folder.",
           call. = FALSE)
    }
    d <- parent
  }
}

PROJECT_ROOT <- find_project_root()

paths <- list(
  root           = PROJECT_ROOT,
  data_raw       = file.path(PROJECT_ROOT, "data", "raw"),
  data_processed = file.path(PROJECT_ROOT, "data", "processed"),
  outputs        = file.path(PROJECT_ROOT, "outputs"),
  figures        = file.path(PROJECT_ROOT, "outputs", "figures"),
  tables         = file.path(PROJECT_ROOT, "outputs", "tables"),
  logs           = file.path(PROJECT_ROOT, "outputs", "logs")
)

files <- list(
  macro_data         = file.path(paths$data_raw, "russian_data.xlsx"),
  consensus_raw      = file.path(paths$data_raw, "consensus_investing.csv"),
  mp_surprise        = file.path(paths$data_processed, "monthly_mp_surprise.csv"),
  analysis_data      = file.path(paths$data_processed, "analysis_data.rds"),
  analysis_logs_csv  = file.path(paths$data_processed, "df_logs.csv"),
  analysis_diff_csv  = file.path(paths$data_processed, "df_diff.csv"),
  estimability       = file.path(paths$outputs, "estimability_report.csv")
)

for (p in paths[-1]) dir.create(p, recursive = TRUE, showWarnings = FALSE)
rm(p)

source(file.path(PROJECT_ROOT, "R", "packages.R"), encoding = "UTF-8")

for (f in c("utils_io.R", "plotting.R", "mp_surprise.R", "data_preparation.R",
            "lp_cholesky.R", "lp_iv.R", "lp_sr_core.R", "lp_sr.R")) {
  source(file.path(PROJECT_ROOT, "R", f), encoding = "UTF-8")
}
rm(f)
