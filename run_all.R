# Runs the whole pipeline: Rscript run_all.R [script numbers, e.g. 10 20]

local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

pipeline <- c(
  "00_build_mp_surprise.R",
  "01_prepare_data.R",
  "10_cholesky_lp.R",
  "20_sign_restrictions_lp.R",
  "30_iv_lp.R",
  "40_lp_nl.R"
)

args    <- if (interactive()) character(0) else commandArgs(trailingOnly = TRUE)
numbers <- grep("^[0-9]+$", args, value = TRUE)
selected <- if (length(numbers)) substr(pipeline, 1, 2) %in% sprintf("%02d", as.integer(numbers)) else rep(TRUE, length(pipeline))
to_run  <- pipeline[selected]
if (!length(to_run)) stop("No script selected. Available: ", paste(pipeline, collapse = ", "))

if (all(selected) && file.exists(files$estimability)) unlink(files$estimability)

run_script <- function(script) {
  log_file <- file.path(paths$logs, sub("\\.R$", ".log", script))
  con <- file(log_file, open = "wt")
  sink(con, split = TRUE)
  on.exit({ sink(); close(con) }, add = TRUE)

  cat(strrep("#", 78), "\n# ", script, "   (", format(Sys.time()), ")\n", strrep("#", 78), "\n", sep = "")
  t0 <- Sys.time()
  withCallingHandlers(
    source(file.path(PROJECT_ROOT, "scripts", script),
           local = new.env(parent = globalenv()), encoding = "UTF-8"),
    warning = function(w) cat("Warning:", conditionMessage(w), "\n")
  )
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("\n[%s finished in %.0f s]\n", script, secs))
  secs
}

timings <- setNames(numeric(length(to_run)), to_run)
for (s in to_run) timings[s] <- run_script(s)

writeLines(capture.output(sessionInfo()), file.path(paths$logs, "session_info.txt"))
cat("\nPipeline finished. Run time by script (s):\n")
print(round(timings))
if (file.exists(files$estimability)) {
  cat("\nEstimability report (outputs/estimability_report.csv):\n")
  print(utils::read.csv(files$estimability, stringsAsFactors = FALSE)[, c("item", "status")], row.names = FALSE)
}
