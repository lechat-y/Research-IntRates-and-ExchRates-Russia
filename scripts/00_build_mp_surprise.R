# 00. Monthly monetary-policy surprise series (actual - consensus) used as the instrument.

if (!exists("PROJECT_ROOT")) local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

# "all": six unscheduled meetings; "flagged3": only 2014-03-03, 2014-12-16, 2022-02-28
unscheduled_set <- "all"

# Effective date of a decision: "announcement" (next calendar day) or "next_bday"
effect_mode <- "announcement"

# Append the 2026-07-24 and 2026-09-11 meetings missing from the raw file
add_missing_2026 <- TRUE

unscheduled_all <- as.Date(c("2014-03-03", "2014-12-16",
                             "2022-02-28", "2022-04-08", "2022-05-26",
                             "2023-08-15"))
unscheduled_3   <- as.Date(c("2014-03-03", "2014-12-16", "2022-02-28"))

ev <- read_consensus_events(files$consensus_raw)

if (add_missing_2026) {
  ev <- add_meetings(ev, tibble::tibble(
    meeting_date = as.Date(c("2026-07-24", "2026-09-11")),
    time = "13:30", actual = c(14, 14), forecast = c(14, 14),
    surprise = c(0, 0), note = NA_character_
  ))
}
ev <- dplyr::arrange(ev, meeting_date)

report_meeting_gaps(ev)

uns_dates <- if (unscheduled_set == "all") unscheduled_all else unscheduled_3
ev <- flag_unscheduled(ev, uns_dates)

cat("\nУдалённые внеплановые заседания:\n")
print(ev |> dplyr::filter(unscheduled) |>
        dplyr::select(meeting_date, time, actual, forecast, surprise))

ev_s    <- dplyr::filter(ev, !unscheduled)
monthly <- aggregate_monthly_surprise(ev_s, ev, effect_mode)

utils::write.csv(monthly, files$mp_surprise, row.names = FALSE)
cat("\nWritten:", files$mp_surprise, "\n")
