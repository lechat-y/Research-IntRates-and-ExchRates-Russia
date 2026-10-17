# I/O helpers, file naming and handling of non-estimable specifications.

load_analysis_data <- function() {
  if (!file.exists(files$analysis_data)) {
    stop("Processed data not found: ", files$analysis_data, "\n",
         "Run scripts/00_build_mp_surprise.R and scripts/01_prepare_data.R ",
         "first (or run_all.R).", call. = FALSE)
  }
  readRDS(files$analysis_data)
}

save_table <- function(x, name) {
  path <- file.path(paths$tables, paste0(name, ".csv"))
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

# Figures are saved as PNG (through ragg if installed)
save_figure <- function(plot, name, width = 6.5, height = 4) {
  png_args <- list(filename = file.path(paths$figures, paste0(name, ".png")),
                   plot = plot, width = width, height = height, dpi = 300)
  if (requireNamespace("ragg", quietly = TRUE)) png_args$device <- ragg::agg_png
  do.call(ggplot2::ggsave, png_args)
  invisible(plot)
}

# <method>_<lags>lags_<model>_<transform>_<sample start>_<dependent variable>[_<variant>]
spec_token <- function(transform, dat, dep) {
  d <- min(dat$date)
  sprintf("%s_%sm%s_%s", transform, format(d, "%Y"), format(d, "%m"), dep)
}

fig_name <- function(method, lags, model, spec, variant = NULL) {
  paste(c(method, sprintf("%dlags", lags), model, spec, variant), collapse = "_")
}

# Signals a specification that cannot be estimated; caught only by attempt()
not_estimable <- function(...) {
  stop(structure(class = c("not_estimable", "error", "condition"),
                 list(message = paste0(...), call = NULL)))
}

attempt <- function(expr) {
  tryCatch(list(value = expr, reason = NA_character_),
           not_estimable = function(e) list(value = NULL, reason = conditionMessage(e)))
}

record_status <- function(item, status, reason = "") {
  cur <- if (file.exists(files$estimability)) {
    utils::read.csv(files$estimability, stringsAsFactors = FALSE)
  } else {
    data.frame(item = character(), status = character(), reason = character(),
               stringsAsFactors = FALSE)
  }
  cur <- cur[cur$item != item, , drop = FALSE]
  cur <- rbind(cur, data.frame(item = item, status = status, reason = reason,
                               stringsAsFactors = FALSE))
  utils::write.csv(cur[order(cur$item), ], files$estimability, row.names = FALSE)
}

check_sample_size <- function(n, lags, n_endog, n_exog, H, what) {
  usable <- n - lags - H
  n_par  <- 1 + (n_endog + n_exog) * lags
  if (usable < 2 * n_par) {
    not_estimable(what, ": sample too short (", n, " months -> ~", usable,
                  " usable observations at h = ", H, " for ", n_par, " regressors per equation).")
  }
  invisible(TRUE)
}

# Saves the table and the figure of one result; results = list(all = ...) or list(pre = ..., post = ...)
# type: "lp" (h, mean, low, up) or "set" (h, lo, hi, med)
emit_irf <- function(name, results, type = c("lp", "set")) {
  type <- match.arg(type)
  ok  <- Filter(function(r) !is.null(r$value), results)
  bad <- Filter(function(r) is.null(r$value), results)
  reasons <- vapply(names(bad), function(n) paste0(n, ": ", bad[[n]]$reason), character(1))

  if (!length(ok)) {
    record_status(name, "NOT ESTIMATED", paste(reasons, collapse = " | "))
    cat(sprintf("[NOT ESTIMATED] %s\n   %s\n", name, paste(reasons, collapse = "\n   ")))
    return(invisible(FALSE))
  }

  if ("all" %in% names(ok)) {
    tab <- ok[["all"]]$value
  } else {
    tab <- dplyr::bind_rows(lapply(names(ok), function(n) {
      d <- ok[[n]]$value
      d$regime <- factor(regime_labels[[n]], levels = regime_labels)
      d
    }))
  }

  p <- if (type == "lp") plot_irf_lp(tab) else plot_irf_set(tab)
  save_table(tab, name)
  save_figure(p, name)

  if (length(bad)) {
    record_status(name, "ESTIMATED (partial)", paste(reasons, collapse = " | "))
    cat(sprintf("[PARTIAL] %s\n   %s\n", name, paste(reasons, collapse = "\n   ")))
  } else {
    record_status(name, "ESTIMATED")
    cat(sprintf("[OK] %s\n", name))
  }
  invisible(TRUE)
}
