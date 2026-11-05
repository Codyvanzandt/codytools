# stan_check(): the sampler's own warning signs for a fit.
#   divergences     should be 0
#   treedepth hits  should be 0 (transitions that hit the maximum treedepth)
#   min E-BFMI      should be above 0.3
# Returns a table with one row per check. R-hat and effective sample size are in stan_summary().
stan_check <- function(fit) {
  sampler_diagnostics(fit)
}

# stan_summary(): mean, sd, 89% interval, R-hat and effective sample sizes, like rethinking's
# precis(). By default only single-number variables are shown (depth = 1); depth = 2 also
# shows vectors, depth = 3 matrices. Name variables to see exactly those, whatever their
# shape (depth is then ignored): stan_summary(fit, "mu"), stan_summary(fit, "ppc_counts").
# lp__ and log_lik are left out unless named.
stan_summary <- function(fit, variables = NULL, depth = 1, prob = 0.89) {
  rlang::check_installed("posterior")
  lo <- (1 - prob) / 2
  hi <- 1 - lo
  s <- fit$summary(
    variables = variables,
    "mean", "sd",
    interval = \(x) posterior::quantile2(x, probs = c(lo, hi)),
    "rhat", "ess_bulk", "ess_tail"
  )
  names(s)[names(s) %in% c(paste0("q", 100 * lo), paste0("q", 100 * hi))] <-
    paste0(c(100 * lo, 100 * hi), "%")
  if (is.null(variables)) s <- s[select_variables(s$variable, depth), ]
  s
}

# stan_trank(): trace rank plots, McElreath's trankplot via bayesplot. If the chains explored
# the posterior equally well, each chain's rank histogram is flat and the lines overlap.
# Which variables are plotted follows the same rules as stan_summary().
stan_trank <- function(fit, variables = NULL, depth = 1, n_bins = 20) {
  rlang::check_installed(c("posterior", "bayesplot"))
  draws <- fit$draws(variables = variables)
  if (is.null(variables)) {
    keep <- select_variables(posterior::variables(draws), depth)
    draws <- posterior::subset_draws(draws, variable = posterior::variables(draws)[keep])
  }
  bayesplot::mcmc_rank_overlay(draws, n_bins = n_bins, ref_line = TRUE)
}

# ---- internals ---------------------------------------------------------------------------

# Which variable names to show, using rethinking's depth rule on the names:
# depth 1 drops anything indexed with [], depth 2 drops matrix elements (a comma).
select_variables <- function(variable, depth) {
  keep <- !(variable == "lp__" | grepl("^log_lik", variable))
  if (depth <= 1) keep <- keep & !grepl("]", variable, fixed = TRUE)
  else if (depth == 2) keep <- keep & !grepl(",", variable, fixed = TRUE)
  keep
}

sampler_diagnostics <- function(fit) {
  d <- tryCatch(fit$diagnostic_summary(quiet = TRUE), error = function(e) NULL)
  divergences <- sum(d$num_divergent)
  treedepth   <- sum(d$num_max_treedepth)
  ebfmi       <- if (length(d$ebfmi)) min(d$ebfmi) else NA_real_
  data.frame(
    check  = c("divergences", "treedepth hits", "min E-BFMI"),
    value  = c(divergences, treedepth, round(ebfmi, 2)),
    status = c(if (divergences > 0) "WARN" else "ok",
               if (treedepth > 0) "WARN" else "ok",
               if (!is.na(ebfmi) && ebfmi < 0.3) "WARN" else "ok"),
    stringsAsFactors = FALSE
  )
}
