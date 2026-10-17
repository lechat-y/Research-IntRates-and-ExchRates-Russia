# Building blocks of LP with sign restrictions: reduced form, exact identified set, Haar draws.

lag_matrix <- function(x, L) {
  x <- as.matrix(x)
  rbind(matrix(NA_real_, L, ncol(x)), x[seq_len(nrow(x) - L), , drop = FALSE])
}

check_consecutive_months <- function(dates) {
  mi <- tryCatch({
    d <- as.Date(dates)
    as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
  }, error = function(e) NULL)
  if (is.null(mi)) {
    warning("Could not parse `date`; month continuity NOT checked.")
  } else if (any(diff(mi) != 1L)) {
    stop("Dates are not consecutive months.")
  }
  invisible(TRUE)
}

prepare_sr_design <- function(df, vars, exog = "urals", lags) {
  dat <- df %>%
    dplyr::select(date, dplyr::all_of(vars), dplyr::all_of(exog)) %>%
    dplyr::arrange(date)
  if (anyNA(dat)) stop("NA inside the sample: fill or shorten the sample first.")
  check_consecutive_months(dat$date)

  Y <- as.matrix(dat[, vars])
  X <- cbind(
    const = 1,
    do.call(cbind, lapply(1:lags, function(L) lag_matrix(Y, L))),
    do.call(cbind, lapply(1:lags, function(L) lag_matrix(dat[[exog]], L)))
  )
  list(dat = dat, Y = Y, X = X, Tn = nrow(Y), vars = vars, K = length(vars))
}

estimate_lp_rf <- function(tt, Y, X, H) {
  K    <- ncol(Y)
  vars <- colnames(Y)

  fit0 <- lm.fit(X[tt, , drop = FALSE], Y[tt, , drop = FALSE])
  if (fit0$rank < ncol(X)) not_estimable("rank-deficient X in the innovation regression.")
  U     <- fit0$residuals
  Sigma <- crossprod(U) / (nrow(U) - ncol(X))
  Sigma <- (Sigma + t(Sigma)) / 2

  G <- array(0, c(K, K, H + 1), dimnames = list(vars, vars, 0:H))
  G[, , 1] <- diag(K)
  Z <- cbind(Y[tt, , drop = FALSE], X[tt, , drop = FALSE])
  for (h in 1:H) {
    fit <- lm.fit(Z, Y[tt + h, , drop = FALSE])
    if (fit$rank < ncol(Z)) not_estimable("rank-deficient regressors in the LP at h = ", h, ".")
    G[, , h + 1] <- t(fit$coefficients[1:K, , drop = FALSE])
  }

  P <- tryCatch(t(chol(Sigma)), error = function(e) NULL)
  if (is.null(P)) not_estimable("Sigma of the innovations is not positive definite.")
  list(G = G, Sigma = Sigma, P = P, U = U)
}

norm_rows <- function(C) {
  nr <- sqrt(rowSums(C^2))
  nr[nr < 1e-300] <- 1
  C / nr
}

sr_idset_sd <- function(G, Sigma, C, resp, H, tol) {
  K <- ncol(C)
  W <- solve(Sigma)
  m <- nrow(C)

  subsets <- c(list(integer(0)),
               unlist(lapply(1:(K - 1), function(k)
                 combn(m, k, simplify = FALSE)), recursive = FALSE))

  faces <- lapply(subsets, function(A) {
    if (length(A) == 0) {
      N <- diag(K)
    } else {
      sv <- svd(C[A, , drop = FALSE], nu = 0, nv = K)
      r  <- sum(sv$d > 1e-10 * max(sv$d, 0))
      if (r >= K) return(NULL)
      N <- sv$v[, (r + 1):K, drop = FALSE]
    }
    M    <- crossprod(N, W %*% N)
    Minv <- tryCatch(solve(M), error = function(e) NULL)
    if (is.null(Minv)) return(NULL)
    list(N = N, M = M, Minv = Minv)
  })
  faces <- faces[!vapply(faces, is.null, logical(1))]

  feasible <- function(dd) all(drop(C %*% dd) <= tol * sqrt(sum(dd^2)))

  out <- matrix(NA_real_, H + 1, 2, dimnames = list(0:H, c("lo", "hi")))
  for (h in 0:H) {
    g   <- G[resp, , h + 1]
    thr <- 1e-20 * drop(crossprod(g, Sigma %*% g))
    lo <- Inf; hi <- -Inf

    for (f in faces) {
      v    <- drop(crossprod(f$N, g))
      val2 <- drop(crossprod(v, f$Minv %*% v))

      if (val2 > thr) {
        delta <- drop(f$N %*% f$Minv %*% v) / sqrt(val2)
      } else if (ncol(f$N) == 1) {
        delta <- drop(f$N) * sqrt(drop(f$Minv))
      } else {
        delta <- drop(f$N[, 1]) / sqrt(f$M[1, 1])
      }

      for (dd in list(delta, -delta)) {
        if (feasible(dd)) {
          val_d <- sum(g * dd)
          hi <- max(hi, val_d)
          lo <- min(lo, val_d)
        }
      }
    }
    if (is.finite(lo)) out[h + 1, ] <- c(lo, hi)
  }
  out
}

sr_haar_draws <- function(P, C, n_target, i_r, chunk, max_draws, tol) {
  K <- nrow(P)
  acc <- list(); n_acc <- 0; n_draw <- 0

  while (n_acc < n_target && n_draw < max_draws) {
    Z <- matrix(rnorm(K * chunk), K)
    Q <- sweep(Z, 2, sqrt(colSums(Z^2)), "/")
    D <- P %*% Q

    keep_nonzero <- abs(D[i_r, ]) > tol * sqrt(colSums(D^2))
    sg <- sign(D[i_r, ]); sg[sg == 0] <- 1
    D  <- sweep(D, 2, sg, "*")

    vals <- C %*% D
    nd   <- sqrt(colSums(D^2))
    ok   <- keep_nonzero &
      (colSums(vals <= rep(tol * nd, each = nrow(C))) == nrow(C))

    n_draw <- n_draw + chunk
    if (any(ok)) {
      acc[[length(acc) + 1]] <- D[, ok, drop = FALSE]
      n_acc <- n_acc + sum(ok)
    }
  }

  if (n_acc == 0) return(list(D = NULL, accept_rate = 0))
  D <- do.call(cbind, acc)[, seq_len(min(n_acc, n_target)), drop = FALSE]
  list(D = D, accept_rate = n_acc / n_draw)
}

sr_irf_resp <- function(G, D, resp, H) {
  out <- sapply(0:H, function(h) drop(G[resp, , h + 1] %*% D))
  if (is.null(dim(out))) out <- matrix(out, nrow = ncol(D))
  t(out)
}

estimate_lp_rf_joint <- function(tt, Y, X, H, reg) {
  K    <- ncol(Y)
  vars <- colnames(Y)
  R    <- reg[tt]
  U    <- matrix(NA_real_, length(tt), K)
  Gs   <- list()

  for (r in 0:1) {
    ir <- which(R == r)
    if (length(ir) < 2 * (K + ncol(X)))
      not_estimable("regime ", c("pre", "post")[r + 1], ": too few shock dates (", length(ir), ").")
    fit0 <- lm.fit(X[tt[ir], , drop = FALSE], Y[tt[ir], , drop = FALSE])
    if (fit0$rank < ncol(X)) not_estimable("rank-deficient X in the innovation regression (regime ", r, ").")
    U[ir, ] <- fit0$residuals

    G <- array(0, c(K, K, H + 1), dimnames = list(vars, vars, 0:H))
    G[, , 1] <- diag(K)
    Z <- cbind(Y[tt[ir], , drop = FALSE], X[tt[ir], , drop = FALSE])
    for (h in 1:H) {
      fit <- lm.fit(Z, Y[tt[ir] + h, , drop = FALSE])
      if (fit$rank < ncol(Z)) not_estimable("rank-deficient regressors in the LP at h = ", h, " (regime ", r, ").")
      G[, , h + 1] <- t(fit$coefficients[1:K, , drop = FALSE])
    }
    Gs[[r + 1]] <- G
  }

  Sigma <- crossprod(U) / (nrow(U) - 2 * ncol(X))
  Sigma <- (Sigma + t(Sigma)) / 2
  P <- tryCatch(t(chol(Sigma)), error = function(e) NULL)
  if (is.null(P)) not_estimable("Sigma of the innovations is not positive definite.")
  list(pre  = list(G = Gs[[1]], Sigma = Sigma, P = P, U = U),
       post = list(G = Gs[[2]], Sigma = Sigma, P = P, U = U))
}
