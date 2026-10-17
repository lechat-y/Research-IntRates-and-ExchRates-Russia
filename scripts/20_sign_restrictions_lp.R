# 20. Local projections with sign restrictions: identified set and median IRF of REER (variants A, B, C).

if (!exists("PROJECT_ROOT")) local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

lags_linear <- 6L
lags_regime <- c(1L, 2L)

cfg <- list(
  vars       = c("ipp", "cpi", "ruonia", "reer"),
  exog       = "urals",
  H          = 12,
  avg_window = 0:12,
  rate_h     = 0:3,
  price_sign = -1,
  n_accept   = 5000,
  min_draws  = 100,
  chunk      = 20000,
  max_draws  = 2e6,
  tol_rel    = 1e-9,
  seed       = 123
)

dat  <- load_analysis_data()$df_logs
spec <- spec_token("levels", dat, "reer")

for (v in sr_variants) {
  nm <- fig_name("signrestr_lp", lags_linear, "linear", spec, v$label)
  emit_irf(nm, list(all = attempt(run_lp_sr(dat, cfg, v, lags_linear))), "set")

  for (p in lags_regime) {
    nm <- fig_name("signrestr_lp", p, "regime2022m03", spec, v$label)
    emit_irf(nm, run_lp_sr_joint(dat, cfg, v, p), "set")
  }
}
