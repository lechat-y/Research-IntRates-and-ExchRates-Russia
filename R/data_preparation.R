# Data preparation functions (used by scripts/01_prepare_data.R).

read_macro_data <- function(path, sample_start, sample_end) {
  df <- openxlsx::read.xlsx(path, sheet = 1)
  df$date <- openxlsx::convertToDate(df$date)
  df %>%
    dplyr::filter(date >= sample_start, date <= sample_end) %>%
    dplyr::arrange(date)
}

# Adds the monthly MP surprise z (p.p.) by calendar month
merge_mp_surprise <- function(df, surprise_csv) {
  mp_surprise <- utils::read.csv(surprise_csv, stringsAsFactors = FALSE) %>%
    dplyr::transmute(date_m = as.Date(date),
                     z      = surprise_w)

  n_before <- nrow(df)
  df <- df %>%
    dplyr::mutate(date_m = lubridate::floor_date(as.Date(date), "month")) %>%
    dplyr::left_join(mp_surprise, by = "date_m") %>%
    dplyr::select(-date_m)

  stopifnot(nrow(df) == n_before)
  if (anyNA(df$z)) stop("No surprise found for some months: check dates.")
  df
}

# X-13ARIMA-SEATS (automatic settings of seasonal::seas)
seasonal_adjust <- function(x, start) {
  as.numeric(seasonal::final(seasonal::seas(stats::ts(x, frequency = 12, start = start))))
}

# Logs of levels (rates in %); IPP and CPI seasonally adjusted; cny = -log(raw CNY/RUB)
build_df_logs <- function(df) {
  ts_start <- c(lubridate::year(min(df$date)), lubridate::month(min(df$date)))

  ipp_chain_sa <- seasonal_adjust(df$ipp,        ts_start)
  cpi_chain_sa <- seasonal_adjust(df$cpi,        ts_start)
  ipp_level_sa <- seasonal_adjust(df$ipp_levels, ts_start)
  cpi_level_sa <- seasonal_adjust(df$cpi_levels, ts_start)

  df %>%
    dplyr::transmute(
      date       = as.Date(date),
      post_2022  = as.integer(date >= as.Date("2022-03-01")),
      neer       = log(neer),
      reer       = log(reer),
      ruonia     = ruonia,
      z          = z,
      ipp_growth = log(ipp_chain_sa / 100),
      ipp        = log(ipp_level_sa),
      cpi_growth = log(cpi_chain_sa / 100),
      cpi        = log(cpi_level_sa),
      urals      = log(urals),
      imports    = log(imports),
      cny        = -log(cny)
    ) %>%
    dplyr::arrange(date) %>%
    tidyr::drop_na()
}

# First differences of df_logs (the surprise z is not differenced)
build_df_diff <- function(df_logs) {
  df_logs %>%
    dplyr::arrange(date) %>%
    dplyr::transmute(
      date      = date,
      post_2022 = post_2022,
      neer      = neer    - dplyr::lag(neer),
      reer      = reer    - dplyr::lag(reer),
      ipp       = ipp     - dplyr::lag(ipp),
      cpi       = cpi     - dplyr::lag(cpi),
      urals     = urals   - dplyr::lag(urals),
      imports   = imports - dplyr::lag(imports),
      cny       = cny     - dplyr::lag(cny),
      ruonia    = ruonia  - dplyr::lag(ruonia),
      z         = z
    ) %>%
    tidyr::drop_na()
}
