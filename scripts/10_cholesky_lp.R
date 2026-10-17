# 10. Cholesky local projections: linear and regime-dependent (before / after March 2022).

if (!exists("PROJECT_ROOT")) local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

H            <- 12
lags_linear  <- 6L
lags_regime  <- c(1L, 2L)

dat_all <- load_analysis_data()
endog   <- c("ipp", "cpi", "ruonia", "reer")

specs <- list(
  list(transform = "levels",  source = "df_logs", from = NULL,                  vars = endog,                            resp = "reer", test = TRUE),
  list(transform = "levels",  source = "df_logs", from = as.Date("2015-01-01"), vars = endog,                            resp = "reer", test = FALSE),
  list(transform = "logdiff", source = "df_diff", from = NULL,                  vars = endog,                            resp = "reer", test = FALSE),
  list(transform = "levels",  source = "df_logs", from = NULL,                  vars = c("ipp", "cpi", "ruonia", "cny"), resp = "cny",  test = FALSE)
)

boot <- list(B = 1000, block = 6, level = 0.95, seed = 123)

as_ok <- function(x) list(value = x, reason = NA_character_)

for (sp in specs) {
  dat <- dat_all[[sp$source]]
  if (!is.null(sp$from)) dat <- dat[dat$date >= sp$from, ]
  spec <- spec_token(sp$transform, dat, sp$resp)

  nm <- fig_name("cholesky_lp", lags_linear, "linear", spec)
  emit_irf(nm, list(all = attempt(chol_lp(dat, sp$vars, sp$resp, lags_linear, H))), "lp")
  if (sp$transform == "logdiff") {
    nm_c <- fig_name("cholesky-lp-cumulative-boot", lags_linear, "linear", spec)
    emit_irf(nm_c, list(all = attempt(chol_cum_boot(dat, sp$vars, sp$resp, lags_linear, H,
                          B = 2000, block = 6, level = 0.95, seed = 123))), "lp")
  }

  for (p in lags_regime) {
    r  <- chol_lp_nl(dat, sp$vars, sp$resp, p)
    nm <- fig_name("cholesky_lp", p, "regime2022m03", spec)
    emit_irf(nm, list(pre = as_ok(r$pre), post = as_ok(r$post)), "lp")

    if (sp$transform == "logdiff") {
      nm_c <- fig_name("cholesky-lp-cumulative-boot", p, "regime2022m03", spec)
      cb <- attempt(chol_cum_boot_nl(dat, sp$vars, sp$resp, p, B = boot$B, block = boot$block,
                                     level = boot$level, seed = boot$seed))
      emit_irf(nm_c, list(pre  = list(value = cb$value$pre,  reason = cb$reason),
                          post = list(value = cb$value$post, reason = cb$reason)), "lp")
    }

    if (sp$test) {
      nm_test <- paste0(nm, "_irf_difference_test")
      tst <- chol_regime_test_nl(r)
      save_table(tst, nm_test)
      record_status(nm_test, "ESTIMATED")
      cat(sprintf("[OK] %s\n", nm_test))
      print(format(tst, digits = 3), row.names = FALSE)
    }
  }
}
