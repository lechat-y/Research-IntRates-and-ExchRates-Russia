# Cholesky local projections (lpirfs): linear and regime-dependent (lp_nl) IRFs, regime test, cumulative IRF with bootstrap.

lp_irf_table <- function(res, resp, shock = NULL, prefix = "irf_lin") {
  pick <- function(type) {
    a <- res[[paste0(prefix, "_", type)]]
    if (is.null(shock)) a[resp, ] else a[resp, , shock]
  }
  mean <- pick("mean")
  data.frame(
    h    = 0:(length(mean) - 1),
    mean = as.numeric(mean),
    low  = as.numeric(pick("low")),
    up   = as.numeric(pick("up"))
  )
}

add_lags <- function(df, vars, p) {
  for (v in vars) for (L in seq_len(p)) df[[paste0(v, "_l", L)]] <- dplyr::lag(df[[v]], L)
  df
}

regime_split <- function(dat, dummy = "post_2022") {
  D <- dat[[dummy]]
  if (!all(D %in% 0:1) || !any(D == 1) || !any(D == 0))
    stop("The regime dummy must take both values 0 and 1 in the sample.")
  brk <- min(which(D == 1))
  if (any(D[brk:length(D)] != 1) || any(D[seq_len(brk - 1)] != 0))
    stop("The regime dummy must look like 0,...,0,1,...,1 (one break).")
  list(pre  = dat[seq_len(brk - 1), , drop = FALSE],
       post = dat[brk:nrow(dat), , drop = FALSE])
}

chol_lp <- function(dat, vars, resp, lags, H = 12, shock = "ruonia", exog = "urals") {
  check_consecutive_months(dat$date)
  check_sample_size(nrow(dat), lags, length(vars), length(exog), H, "Cholesky LP")

  res <- lpirfs::lp_lin(
    endog_data     = as.data.frame(dat[, vars, drop = FALSE]),
    lags_endog_lin = lags,
    trend          = 0,
    confint        = 1.96,
    shock_type     = 1,
    hor            = H,
    use_nw         = TRUE,
    exog_data      = as.data.frame(dat[, exog, drop = FALSE]),
    lags_exog      = lags
  )
  lp_irf_table(res, resp = match(resp, vars), shock = match(shock, vars))
}

chol_pmw_irf <- function(dat, vars, resp, lags, H = 12, shock = "ruonia", exog = "urals") {
  check_consecutive_months(dat$date)
  check_sample_size(nrow(dat), lags, length(vars), length(exog), H, "LP with contemporaneous controls")

  d <- add_lags(as.data.frame(dat), c(vars, exog), lags)
  contemp <- vars[seq_len(match(shock, vars) - 1)]
  rhs <- c(shock, contemp,
           paste0(rep(c(vars, exog), each = lags), "_l", seq_len(lags)))

  out <- data.frame(h = 0:H, beta = NA_real_, se = NA_real_)
  for (h in 0:H) {
    d$.y <- dplyr::lead(d[[resp]], h)
    fit  <- stats::lm(stats::reformulate(rhs, response = ".y"), data = d)
    if (anyNA(stats::coef(fit)))
      not_estimable("rank-deficient regression at h = ", h, " (collinear regressors).")
    if (stats::nobs(fit) < 2 * length(stats::coef(fit)))
      not_estimable("too few observations at h = ", h, " (", stats::nobs(fit), " for ",
                    length(stats::coef(fit)), " coefficients).")
    V <- sandwich::NeweyWest(fit, lag = h + 1, prewhite = FALSE, adjust = TRUE)
    out$beta[h + 1] <- stats::coef(fit)[[shock]]
    out$se[h + 1]   <- sqrt(V[shock, shock])
  }
  out
}

chol_regime_test <- function(dat, vars, resp, lags, H = 12) {
  sp   <- regime_split(dat)
  pre  <- chol_pmw_irf(sp$pre,  vars, resp, lags, H)
  post <- chol_pmw_irf(sp$post, vars, resp, lags, H)
  diff <- post$beta - pre$beta
  se   <- sqrt(pre$se^2 + post$se^2)
  data.frame(h = pre$h,
             IRF_pre = pre$beta, IRF_post = post$beta,
             difference = diff, SE_difference = se,
             p_value = 2 * stats::pnorm(-abs(diff / se)))
}

chol_cum_boot <- function(dat, vars, resp, lags, H = 12, shock = "ruonia", exog = "urals",
                          B = 2000, block = 6, level = 0.95, seed = 123) {
  check_consecutive_months(dat$date)
  check_sample_size(nrow(dat), lags, length(vars), length(exog), H, "Cumulative Cholesky LP")

  d <- add_lags(as.data.frame(dat), c(vars, exog), lags)
  contemp <- vars[seq_len(match(shock, vars) - 1)]
  rhs <- c(shock, contemp, paste0(rep(c(vars, exog), each = lags), "_l", seq_len(lags)))

  Yl <- sapply(0:H, function(h) dplyr::lead(d[[resp]], h))
  X  <- cbind(const = 1, as.matrix(d[, rhs, drop = FALSE]))
  ok <- stats::complete.cases(X, Yl)
  X  <- X[ok, , drop = FALSE]; Yl <- Yl[ok, , drop = FALSE]
  n  <- nrow(X)
  if (n < 2 * ncol(X) || n < 2 * block)
    not_estimable("too few LP rows for the bootstrap (", n, " rows, ", ncol(X), " regressors, block = ", block, ").")

  est <- function(idx) {
    Xi  <- X[idx, , drop = FALSE]
    fit <- stats::lm.fit(Xi, Yl[idx, , drop = FALSE])
    if (fit$rank < ncol(Xi)) return(NULL)
    cumsum(fit$coefficients[shock, ])
  }

  point <- est(seq_len(n))
  if (is.null(point)) not_estimable("rank-deficient LP regressors (collinear).")

  set.seed(seed)
  nb <- ceiling(n / block)
  draws <- matrix(NA_real_, B, H + 1)
  for (b in seq_len(B)) {
    st  <- sample.int(n - block + 1, nb, replace = TRUE)
    idx <- unlist(lapply(st, function(s) s:(s + block - 1)))[seq_len(n)]
    e <- est(idx)
    if (!is.null(e)) draws[b, ] <- e
  }
  good <- stats::complete.cases(draws)
  if (mean(good) < 0.9)
    not_estimable("more than 10% of bootstrap resamples gave rank-deficient regressors.")

  a  <- (1 - level) / 2
  qs <- apply(draws[good, , drop = FALSE], 2, stats::quantile, probs = c(a, 1 - a))
  data.frame(h = 0:H, mean = as.numeric(point), low = qs[1, ], up = qs[2, ], row.names = NULL)
}

chol_lp_nl <- function(params, vars, resp, lags, shock = "ruonia", exog = "urals") {
  check_consecutive_months(params$date)
  res_nl <- lpirfs::lp_nl(
    endog_data = as.data.frame(params[, vars, drop = FALSE]),

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

    exog_data = as.data.frame(params[, exog, drop = FALSE]),

    lags_exog = lags
  )
  list(pre  = lp_irf_table(res_nl, match(resp, vars), match(shock, vars), prefix = "irf_s1"),
       post = lp_irf_table(res_nl, match(resp, vars), match(shock, vars), prefix = "irf_s2"))
}

cumulate_irf <- function(tab) {
  data.frame(h = tab$h, mean = cumsum(tab$mean), low = cumsum(tab$low), up = cumsum(tab$up))
}

chol_regime_test_nl <- function(r, confint = 1.96) {
  se <- function(t) (t$up - t$low) / (2 * confint)
  diff <- r$post$mean - r$pre$mean
  sd   <- sqrt(se(r$pre)^2 + se(r$post)^2)
  data.frame(h = r$pre$h, IRF_pre = r$pre$mean, IRF_post = r$post$mean,
             difference = diff, SE_difference = sd,
             p_value = 2 * stats::pnorm(-abs(diff / sd)))
}

chol_cum_boot_nl <- function(params, vars, resp, lags, B = 1000, block = 6,
                             level = 0.95, seed = 123) {
  base <- chol_lp_nl(params, vars, resp, lags)
  pt   <- lapply(base, function(t) cumsum(t$mean))
  n <- nrow(params); nb <- ceiling(n / block)
  set.seed(seed)
  draws <- list(pre = list(), post = list())
  for (b in seq_len(B)) {
    st  <- sample.int(n - block + 1, nb, replace = TRUE)
    idx <- unlist(lapply(st, function(s) s:(s + block - 1)))[seq_len(n)]
    d   <- params[idx, , drop = FALSE]
    d$date <- params$date
    r <- tryCatch(suppressWarnings(suppressMessages(chol_lp_nl(d, vars, resp, lags))),
                  error = function(e) NULL)
    if (is.null(r)) next
    draws$pre[[length(draws$pre) + 1]]   <- cumsum(r$pre$mean)
    draws$post[[length(draws$post) + 1]] <- cumsum(r$post$mean)
  }
  a <- (1 - level) / 2
  one <- function(k) {
    if (length(draws[[k]]) < 0.9 * B)
      not_estimable("more than 10% of bootstrap resamples failed in lp_nl (", k, ").")
    M  <- do.call(rbind, draws[[k]])
    qs <- apply(M, 2, stats::quantile, probs = c(a, 1 - a))
    data.frame(h = base[[k]]$h, mean = pt[[k]], low = qs[1, ], up = qs[2, ], row.names = NULL)
  }
  list(pre = one("pre"), post = one("post"))
}
