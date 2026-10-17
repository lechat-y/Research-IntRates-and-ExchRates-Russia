# 40. lpirfs::lp_nl: nonlinear LP with a binary regime indicator (post_2022); figures rc_l1..rc_l3.

if (!exists("PROJECT_ROOT")) local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

params <- load_analysis_data()$df_logs
check_consecutive_months(params$date)

for (lags in 1:3) {
  res_nl <- lp_nl(
    endog_data = params %>%
      dplyr::select(
        ipp,
        cpi,
        ruonia,
        reer
      ),

    lags_endog_lin = lags,
    lags_endog_nl  = lags,

    trend       = 0,
    shock_type  = 1,
    confint     = 1.96,
    hor         = 12,
    use_nw      = TRUE,

    switching     = params$post_2022,
    use_logistic  = FALSE,
    lag_switching = FALSE,

    exog_data = params %>%
      dplyr::select(
        urals
      ),

    lags_exog = lags
  )

  i_ruonia <- 3L
  i_reer   <- 4L
  ok <- function(prefix) list(value = lp_irf_table(res_nl, resp = i_reer, shock = i_ruonia,
                                                   prefix = prefix),
                              reason = NA_character_)
  emit_irf(sprintf("rc_l%d", lags), list(pre = ok("irf_s1"), post = ok("irf_s2")), "lp")
}
