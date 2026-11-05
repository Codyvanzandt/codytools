# codytools

A warm literary ggplot2 theme, and helpers for sending plots from analysis projects to a Quarto website.

```r
pak::pak("Codyvanzandt/codytools")
options(codytools.site = "C:/Users/codya/Desktop/personal-website")   # in ~/.Rprofile
```

## Plotting

```r
library(codytools)
cody_use()      # fonts, theme, default palettes and geom styles
```

Plain ggplot code then picks up the theme and palettes. Choices that stay explicit:
`scale_fill_cody_div()` for values centred on zero, and
`scale_colour_cody(values = c(Control = "burgundy", Treatment = "blue"))` to keep a
category's colour fixed across figures.

## Stan

A small workflow around `cmdstanr`, with models written by hand in `stan/` (needs `cmdstanr`
and `posterior`; `bayesplot` for rank plots).

```r
stan_new("my_model")                                  # skeleton stan/my_model.stan with a prior_only switch
fit <- stan_fit("my_model", stan_data)                # saved in fits/, reused until code, data or settings change
fit <- stan_fit("my_model", stan_data, adapt_delta = 0.99)    # anything else goes to model$sample()
pri <- stan_fit("my_model", stan_data, prior_only = TRUE)     # sample the priors alone

stan_check(fit)       # divergences, treedepth hits, E-BFMI
stan_summary(fit)     # mean, sd, 89% interval, rhat, ess_bulk, ess_tail (like rethinking's precis)
stan_trank(fit)       # trace rank plots
```

Defaults: seed 8, 4 chains, 1000 warmup + 1000 sampling, `adapt_delta = 0.95`. `stan_fit()` warns
on divergences, treedepth hits and low E-BFMI. `stan_summary()` shows single-number variables
(`depth = 1`); `depth = 2` adds vectors, or name variables: `stan_summary(fit, "mu")`. For draws use
`tidybayes::spread_draws()`.

`prior_only = TRUE` needs `int<lower=0, upper=1> prior_only;` in the data block and the likelihood
skipped when it is 1 (`stan_new()` writes that).

## Posts

```r
to_post(my_fig, "my-slug")      # from an analysis project; saved as posts/my-slug/my_fig.rds
new_post("My *title*")          # creates posts/my-title/index.qmd (or pass slug =)
from_post("my_fig")             # inside the post
build_site()                    # quarto render into docs/
publish_site("Add post")        # build, then commit and push
```
