# LP-IV (2SLS) with the monthly MP surprise as instrument; IRF is produced only if the first-stage F >= min_F.

iv_first_stage_F <- function(dat, vars, shock, instr, exog, lags) {
  d   <- add_lags(as.data.frame(dat), c(vars, exog, instr), lags)
  rhs <- c(instr, paste0(rep(c(vars, exog, instr), each = lags), "_l", seq_len(lags)))
  if (all(dat[[instr]] == 0)) not_estimable("the instrument is identically zero in this sample.")

  fit <- stats::lm(stats::reformulate(rhs, response = shock), data = d)
  if (anyNA(stats::coef(fit))) not_estimable("rank-deficient first stage (collinear controls / instrument).")
  V <- sandwich::NeweyWest(fit, prewhite = FALSE, adjust = TRUE)
  b <- stats::coef(fit)[[instr]]
  c(F = (b / sqrt(V[instr, instr]))^2, n = stats::nobs(fit))
}

iv_lp <- function(dat, vars, resp, lags, H = 12, shock = "ruonia", instr = "z",
                  exog = "urals", min_F = 10) {
  check_consecutive_months(dat$date)
  check_sample_size(nrow(dat), lags, length(vars), length(exog) + 1L, H + 1L, "LP-IV")

  fs <- iv_first_stage_F(dat, vars, shock, instr, exog, lags)
  if (fs[["F"]] < min_F)
    not_estimable(sprintf(paste0("weak instrument: HAC first-stage F of z = %.1f (n = %d) is below %.0f; ",
                                 "the IV IRF and its bands are not reliable."),
                          fs[["F"]], as.integer(fs[["n"]]), min_F))

  exog_df <- data.frame(as.data.frame(dat[, exog, drop = FALSE]), z_ctrl = dat[[instr]])
  res <- lpirfs::lp_lin_iv(
    endog_data     = as.data.frame(dat[, vars, drop = FALSE]),
    shock          = as.data.frame(dat[, shock, drop = FALSE]),
    instrum        = as.data.frame(dat[, instr, drop = FALSE]),
    use_twosls     = TRUE,
    lags_endog_lin = lags,
    trend          = 0,
    confint        = 1.96,
    hor            = H + 1,
    use_nw         = TRUE,
    exog_data      = exog_df,
    lags_exog      = lags
  )
  lp_irf_table(res, resp = match(resp, vars))
}
