# Local projections with sign restrictions: restriction variants, identified set and median of the REER IRF.

sr_variants <- list(
  A = list(label = "restrA-irf-fullrule",    policy = c("ipp", "cpi", "reer")),
  B = list(label = "restrB-irf-only",        policy = character(0)),
  C = list(label = "restrC-irf-partialrule", policy = c("ipp", "cpi"))
)

run_lp_sr <- function(dat, cfg, variant, lags) {
  d <- sr_design_sample(dat, cfg, lags)
  sr_from_rf(estimate_lp_rf(d$tt, d$Y, d$X, cfg$H), cfg, variant)
}

run_lp_sr_joint <- function(dat, cfg, variant, lags) {
  dd <- attempt(sr_design_sample(dat, cfg, lags))
  if (is.null(dd$value)) return(list(pre = dd, post = dd))
  d  <- dd$value
  rf <- attempt(estimate_lp_rf_joint(d$tt, d$Y, d$X, cfg$H, as.integer(dat$post_2022[order(dat$date)])))
  if (is.null(rf$value)) return(list(pre = rf, post = rf))
  list(pre  = attempt(sr_from_rf(rf$value$pre,  cfg, variant)),
       post = attempt(sr_from_rf(rf$value$post, cfg, variant)))
}

sr_design_sample <- function(dat, cfg, lags) {
  vars <- cfg$vars; K <- length(vars); H <- cfg$H
  design <- prepare_sr_design(dat, vars, cfg$exog, lags)
  tt <- which(complete.cases(design$X))
  tt <- tt[tt + H <= design$Tn]
  n_reg <- K + ncol(design$X)
  if (length(tt) < 2 * n_reg)
    not_estimable("sample too short: ", length(tt), " shock dates for ", n_reg,
                  " regressors in the LP (lags = ", lags, ", H = ", H, ").")
  list(Y = design$Y, X = design$X, tt = tt)
}

sr_from_rf <- function(rf, cfg, variant) {
  set.seed(cfg$seed)
  vars <- cfg$vars; K <- length(vars); H <- cfg$H
  i_ipp <- match("ipp", vars); i_cpi <- match("cpi", vars)
  i_r   <- match("ruonia", vars); i_reer <- match("reer", vars)
  stopifnot(H >= max(cfg$avg_window, cfg$rate_h))

  irf_rows <- function(G) {
    avg  <- function(i) rowMeans(sapply(cfg$avg_window, function(h) G[i, , h + 1]))
    rows <- list(-cfg$price_sign * avg(i_ipp), -cfg$price_sign * avg(i_cpi))
    for (h in setdiff(cfg$rate_h, 0)) rows <- c(rows, list(-G[i_r, , h + 1]))
    do.call(rbind, rows)
  }
  policy_rows <- function(Sigma) {
    if (!length(variant$policy)) return(NULL)
    W <- unname(solve(Sigma))
    rows <- list(-W[i_r, ])
    if ("ipp"  %in% variant$policy) rows <- c(rows, list( W[i_ipp, ]))
    if ("cpi"  %in% variant$policy) rows <- c(rows, list( W[i_cpi, ]))
    if ("reer" %in% variant$policy) rows <- c(rows, list(-W[i_reer, ]))
    do.call(rbind, rows)
  }
  build_C <- function(include_r_sign) {
    C <- rbind(unname(irf_rows(rf$G)), policy_rows(rf$Sigma))
    if (include_r_sign) C <- rbind(C, -diag(K)[i_r, ])
    norm_rows(C)
  }

  is_sd <- sr_idset_sd(rf$G, rf$Sigma, build_C(TRUE), i_reer, H, cfg$tol_rel)
  if (anyNA(is_sd))
    not_estimable("the identified set is EMPTY at the point estimate: the restrictions (",
                  variant$label, ") are incompatible with the estimated (G, Sigma).")

  hd <- sr_haar_draws(rf$P, build_C(FALSE), cfg$n_accept, i_r, cfg$chunk, cfg$max_draws, cfg$tol_rel)
  if (is.null(hd$D) || ncol(hd$D) < cfg$min_draws)
    not_estimable("the median of the identified set is not defined: only ",
                  if (is.null(hd$D)) 0 else ncol(hd$D), " admissible Haar draws among ",
                  format(cfg$max_draws, scientific = FALSE), " (the set is empty or of measure ~0).")

  reer_sd <- sr_irf_resp(rf$G, hd$D, i_reer, H)
  sc <- max(abs(c(is_sd)), 1e-12)
  if (any(apply(reer_sd, 1, min) < is_sd[, "lo"] - 1e-6 * sc) ||
      any(apply(reer_sd, 1, max) > is_sd[, "hi"] + 1e-6 * sc))
    not_estimable("numerical inconsistency: admissible draws fall outside the exact identified set.")

  data.frame(h = 0:H, lo = is_sd[, "lo"], hi = is_sd[, "hi"], med = apply(reer_sd, 1, median),
             row.names = NULL)
}
