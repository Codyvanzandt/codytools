# stan_fit(): fit a Stan model, or reuse a saved fit when nothing that affects the
# posterior has changed.
#
#   fit <- stan_fit("negbin_pooled", stan_data)
#   fit <- stan_fit("negbin_hier", hier_data, adapt_delta = 0.99)
#   pri <- stan_fit("negbin_pooled", stan_data, prior_only = TRUE)
#
# `model` is a name (stan/<name>.stan under `stan_dir`) or a cmdstanr model. Anything in `...`
# goes to model$sample(). A saved fit is reused only when all of these are unchanged:
#   - the Stan code, including any #include files
#   - the data passed to Stan
#   - the sampler settings (seed, chains, iterations, adapt_delta, init, ...)
#   - the C++ compiler options and the CmdStan version
# Output-only settings (refresh, show_messages, parallel_chains, ...) are ignored.
#
# Fits are saved with $save_object() in `fits_dir`, one file per version, so reverting a change
# reuses the earlier fit. Delete the folder at any time to start fresh. The model is compiled
# only when it actually needs to sample.
#
# prior_only = TRUE samples the priors alone. The Stan file must declare a data variable
# `int<lower=0, upper=1> prior_only;` and skip its likelihood when it is 1 (stan_new() writes
# that for you). stan_fit() sets the variable, so the data list stays the same.
#
# Warns when the sampler reports divergences, max-treedepth hits or low E-BFMI.
stan_fit <- function(model, data, ...,
                     seed = 8L, chains = 4, parallel_chains = chains,
                     iter_warmup = 1000, iter_sampling = 1000, adapt_delta = 0.95,
                     refresh = 0, show_messages = FALSE, show_exceptions = FALSE,
                     prior_only = FALSE, stan_dir = "stan", fits_dir = "fits") {
  rlang::check_installed(c("cmdstanr", "posterior"))
  model <- as_stan_model(model, stan_dir)
  code <- stan_code(model$stan_file(), model$include_paths())

  if (declares_prior_only(code)) {
    if (!is.list(data)) stop("stan_fit(): pass `data` as a list so prior_only can be set.", call. = FALSE)
    if (is.null(data$prior_only) || isTRUE(prior_only)) data$prior_only <- as.integer(prior_only)
  } else if (isTRUE(prior_only)) {
    stop("stan_fit(): prior_only = TRUE needs `int<lower=0, upper=1> prior_only;` in the data block ",
         "of ", basename(model$stan_file()), " and the likelihood skipped when it is 1. ",
         "stan_new() writes a model with that built in.", call. = FALSE)
  }

  args <- c(list(data = data, seed = as.integer(seed), chains = chains, parallel_chains = parallel_chains,
                 iter_warmup = iter_warmup, iter_sampling = iter_sampling, adapt_delta = adapt_delta,
                 refresh = refresh, show_messages = show_messages, show_exceptions = show_exceptions),
            list(...))

  output_only <- c("refresh", "show_messages", "show_exceptions", "parallel_chains",
                   "threads_per_chain", "output_dir", "output_basename", "diagnostics")
  settings <- args[setdiff(names(args), c("data", output_only))]
  settings <- lapply(settings, \(x) if (is.function(x)) deparse(x) else x)

  key_data <- data
  if (is.character(key_data) && length(key_data) == 1 && file.exists(key_data)) key_data <- readLines(key_data)

  key <- rlang::hash(list(
    code     = code,
    data     = key_data,
    settings = settings,
    compiler = model$cpp_options(),
    cmdstan  = as.character(cmdstanr::cmdstan_version())
  ))
  name <- tools::file_path_sans_ext(basename(model$stan_file()))
  path <- file.path(fits_dir, paste0(name, "-", substr(key, 1, 12), ".rds"))

  if (file.exists(path)) {
    message("stan_fit(): reusing saved fit for ", name, " (seed ", args$seed, ")")
    fit <- readRDS(path)
  } else {
    message("stan_fit(): fitting ", name, " (seed ", args$seed, ")")
    model$compile(quiet = TRUE)                  # no-op if the executable is already up to date
    fit <- do.call(model$sample, args)
    if (all(fit$return_codes() == 0)) {          # never save a run where a chain failed
      dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
      fit$save_object(path)
    } else {
      warning("stan_fit(): a chain failed, so this fit was not saved", call. = FALSE)
    }
  }

  problems <- sampler_diagnostics(fit)
  problems <- problems[problems$status != "ok", ]
  if (nrow(problems)) {
    warning("stan_fit(): ", name, ": ", paste(sprintf("%s = %s", problems$check, problems$value), collapse = "; "),
            ". See stan_check().", call. = FALSE)
  }
  fit
}

# stan_new(): write a skeleton stan/<name>.stan with the prior_only switch and a
# frequency-table likelihood (one row per group x outcome, with its count n).
stan_new <- function(name, dir = "stan") {
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", name))
    stop("stan_new(): name must be letters, digits and underscores, e.g. \"negbin_pooled\".", call. = FALSE)
  path <- file.path(dir, paste0(name, ".stan"))
  if (file.exists(path)) stop(path, " already exists; nothing was overwritten.", call. = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    paste0("// ", name, ": say in one line how a single outcome is generated."),
    "//   Y_ig ~ Poisson(mu_i)",
    "// Data are a frequency table (one row per group x outcome, with its count n).",
    "data {",
    "  int<lower=0, upper=1> prior_only;   // 1 = skip the likelihood and sample the priors (stan_fit() sets this)",
    "  int<lower=1> M;                     // rows in the frequency table",
    "  int<lower=1> K;                     // groups",
    "  array[M] int<lower=1, upper=K> group;",
    "  array[M] int<lower=0> y;",
    "  array[M] int<lower=1> n;",
    "}",
    "",
    "parameters {",
    "  vector[K] log_mu;",
    "}",
    "",
    "model {",
    "  log_mu ~ normal(0, 1);",
    "  if (!prior_only) {",
    "    for (m in 1:M) {",
    "      target += n[m] * poisson_log_lpmf(y[m] | log_mu[group[m]]);",
    "    }",
    "  }",
    "}",
    "",
    "generated quantities {",
    "  vector[K] mu = exp(log_mu);",
    "  // replicated data for posterior (and, with prior_only, prior) predictive checks",
    "}"
  ), path)
  message("Created ", path)
  invisible(path)
}

# ---- internals ---------------------------------------------------------------------------

as_stan_model <- function(model, stan_dir) {
  if (inherits(model, "CmdStanModel")) return(model)
  if (!is.character(model) || length(model) != 1L)
    stop("stan_fit(): `model` must be a model name or a cmdstanr model.", call. = FALSE)
  file <- if (grepl("\\.stan$", model)) model else file.path(stan_dir, paste0(model, ".stan"))
  if (!file.exists(file)) stop("stan_fit(): ", file, " not found.", call. = FALSE)
  cmdstanr::cmdstan_model(file, compile = FALSE)
}

declares_prior_only <- function(code) {
  any(grepl("\\bint\\b[^;]*\\bprior_only\\b", sub("//.*", "", code)))
}

# Stan code plus the code of any #include files, recursively
stan_code <- function(file, include_paths = NULL) {
  code <- readLines(file)
  inc <- gsub('^\\s*#include\\s*[<"]?|[>"]?\\s*$', "", grep("^\\s*#include", code, value = TRUE))
  inc_files <- unlist(lapply(inc, \(f) {
    hits <- file.path(c(dirname(file), include_paths), f)
    hits[file.exists(hits)][1]
  }))
  c(code, unlist(lapply(inc_files, stan_code, include_paths = include_paths)))
}
