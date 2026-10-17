# 01. Analysis data set: macro data + surprise, X-13 seasonal adjustment, logs, differences.

if (!exists("PROJECT_ROOT")) local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

sample_start <- as.Date("2013-01-01")
sample_end   <- as.Date("2026-01-01")

df <- read_macro_data(files$macro_data, sample_start, sample_end)
df <- merge_mp_surprise(df, files$mp_surprise)

df_logs <- build_df_logs(df)
df_diff <- build_df_diff(df_logs)

check_consecutive_months(df_logs$date)
check_consecutive_months(df_diff$date)

saveRDS(list(df = df, df_logs = df_logs, df_diff = df_diff), files$analysis_data)
utils::write.csv(df_logs, files$analysis_logs_csv, row.names = FALSE)
utils::write.csv(df_diff, files$analysis_diff_csv, row.names = FALSE)

cat(sprintf("df_logs: %d months (%s .. %s); df_diff: %d months\n",
            nrow(df_logs), format(min(df_logs$date)), format(max(df_logs$date)),
            nrow(df_diff)))
cat("Written:", files$analysis_data, "\n")
