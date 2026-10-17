# Monetary-policy surprises (actual - consensus) aggregated to a monthly series
# with day weights (used by scripts/00_build_mp_surprise.R).

# Event file: separator ";", encoding CP1251, dates m/d/yy
read_consensus_events <- function(path) {
  raw <- utils::read.csv(path, sep = ";", fileEncoding = "CP1251",
                         stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
  names(raw)[1:6] <- c("meeting_date", "month_start", "time",
                       "actual", "forecast", "surprise")
  note_col <- if (ncol(raw) >= 7) as.character(raw[[7]]) else rep(NA_character_, nrow(raw))

  ev <- tibble::tibble(
    meeting_date = as.Date(raw$meeting_date, format = "%m/%d/%y"),
    time         = as.character(raw$time),
    actual       = as.numeric(raw$actual),
    forecast     = as.numeric(raw$forecast),
    surprise     = as.numeric(raw$surprise),
    note         = note_col
  )

  stopifnot(!anyNA(ev$meeting_date), !anyNA(ev$surprise))
  if (max(abs(ev$actual - ev$forecast - ev$surprise)) > 1e-9)
    warning("В файле surprise != actual - forecast хотя бы в одной строке: проверьте данные.")
  ev
}

add_meetings <- function(ev, extra) {
  extra <- extra %>% dplyr::filter(!(meeting_date %in% ev$meeting_date))
  dplyr::bind_rows(ev, extra)
}

report_meeting_gaps <- function(ev, min_gap = 45) {
  cat("\nИнтервалы между заседаниями > 45 дней (месяцы без решения -> сюрприз = 0):\n")
  print(ev |>
          dplyr::mutate(prev_meeting = dplyr::lag(meeting_date),
                        gap_days = as.numeric(meeting_date - prev_meeting)) |>
          dplyr::filter(gap_days > min_gap) |>
          dplyr::select(prev_meeting, meeting_date, gap_days), n = 50)
}

flag_unscheduled <- function(ev, unscheduled_dates) {
  pat_unsched <- "внеплан"
  ev |>
    dplyr::mutate(unscheduled = meeting_date %in% unscheduled_dates |
                    grepl(pat_unsched, note, ignore.case = TRUE))
}

# Date from which a decision is in force: next calendar day or next business day
effective_date <- function(d, mode) {
  if (mode == "announcement") return(d + 1)
  wd <- lubridate::wday(d, week_start = 1)
  d + ifelse(wd >= 5, 8 - wd, 1)
}

# Day-weighted monthly aggregation (Kuttner 2001 / Gertler-Karadi 2015); months without a meeting = 0
aggregate_monthly_surprise <- function(ev_s, ev_all, effect_mode) {
  alloc <- ev_s |>
    dplyr::mutate(eff    = effective_date(meeting_date, effect_mode),
                  m0     = lubridate::floor_date(eff, "month"),
                  D      = lubridate::days_in_month(eff),
                  k      = lubridate::day(eff),
                  w_cur  = (D - k + 1) / D,
                  w_next = (k - 1) / D)

  contrib <- dplyr::bind_rows(
    dplyr::transmute(alloc, date = m0,                                   v = surprise * w_cur),
    dplyr::transmute(alloc, date = lubridate::`%m+%`(m0, months(1)),     v = surprise * w_next)
  )

  monthly_nz <- contrib |>
    dplyr::group_by(date) |>
    dplyr::summarise(surprise_w = sum(v), .groups = "drop")

  grid <- tibble::tibble(date = seq(
    lubridate::floor_date(min(ev_all$meeting_date), "month"),
    lubridate::`%m+%`(lubridate::floor_date(max(ev_all$meeting_date), "month"), months(1)),
    by = "month"))

  n_meet <- ev_s |>
    dplyr::mutate(date = lubridate::floor_date(meeting_date, "month")) |>
    dplyr::count(date, name = "n_meetings")

  monthly <- grid |>
    dplyr::left_join(monthly_nz, by = "date") |>
    dplyr::left_join(n_meet,     by = "date") |>
    dplyr::mutate(surprise_w = dplyr::coalesce(surprise_w, 0),
                  n_meetings = dplyr::coalesce(n_meetings, 0L),
                  year       = lubridate::year(date),
                  month      = lubridate::month(date)) |>
    dplyr::select(year, month, date, surprise_w, n_meetings)

  stopifnot(abs(sum(monthly$surprise_w) - sum(ev_s$surprise)) < 1e-9)
  monthly
}
