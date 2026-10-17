# 30. LP-IV (2SLS): RUONIA instrumented by the monthly MP surprise z.

if (!exists("PROJECT_ROOT")) local({
  d <- getwd()
  while (!file.exists(file.path(d, "run_all.R")) && dirname(d) != d) d <- dirname(d)
  source(file.path(d, "R", "setup.R"), local = globalenv())
})

H           <- 12
min_F       <- 10
lags_linear <- 6L
lags_regime <- c(1L, 2L)

dat   <- load_analysis_data()$df_logs
endog <- c("ipp", "cpi", "ruonia", "reer")
spec  <- spec_token("levels", dat, "reer")
sub   <- regime_split(dat)

nm <- fig_name("iv_lp", lags_linear, "linear", spec)
emit_irf(nm, list(all = attempt(iv_lp(dat, endog, "reer", lags_linear, H, min_F = min_F))), "lp")

for (p in lags_regime) {
  nm <- fig_name("iv_lp", p, "regime2022m03", spec)
  emit_irf(nm, list(
    pre  = attempt(iv_lp(sub$pre,  endog, "reer", p, H, min_F = min_F)),
    post = attempt(iv_lp(sub$post, endog, "reer", p, H, min_F = min_F))), "lp")
}
