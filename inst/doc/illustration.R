## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    fig.align = "center",
    collapse = TRUE,
    comment = "#>",
    warning = FALSE,
    message = FALSE,
    cache = FALSE,
    dev.args = list(bg = "transparent"),
    out.width = 600,
    crop = NULL
)

knitr::knit_hooks$set(output = multimedia::ansi_aware_handler)
suppressPackageStartupMessages(library(ggplot2))
options(
    ggplot2.discrete.colour = c(
        "#9491D9", "#F24405", "#3F8C61", "#8C2E62", "#F2B705", "#11A0D9"
    ),
    ggplot2.discrete.fill = c(
        "#9491D9", "#F24405", "#3F8C61", "#8C2E62", "#F2B705", "#11A0D9"
    ),
    ggplot2.continuous.colour = function(...) {
        scale_color_distiller(palette = "Spectral", ...)
    },
    ggplot2.continuous.fill = function(...) {
        scale_fill_distiller(palette = "Spectral", ...)
    },
    crayon.enabled = TRUE
)

th <- theme_classic() +
    theme(
        panel.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.position = "bottom"
    )
theme_set(th)

## ----setup--------------------------------------------------------------------
library(glue)
library(tidyverse)
library(multimedia)
set.seed(20240111)

#' Helper to plot the real data
plot_exper <- function(fit, profile) {
    pivot_samples(fit, profile) |>
        ggplot() +
        geom_point(aes(mediator, value, col = factor(treatment))) +
        facet_grid(. ~ outcome) +
        theme(legend.position = "bottom")
}

#' Convert a SummarizedExperiment to long form
pivot_samples <- function(fit, profile) {
    exper_sample <- sample(fit, profile = profile)
    outcomes(exper_sample) |>
        bind_cols(mediators(exper_sample), t_outcome) |>
        pivot_longer(starts_with("outcome"), names_to = "outcome")
}

## ----prepare_data-------------------------------------------------------------
xy_data <- demo_spline()
xy_long <- list(
    true = pivot_longer(xy_data, starts_with("outcome"), names_to = "outcome")
)

## ----fit_model----------------------------------------------------------------
exper <- mediation_data(
    xy_data, starts_with("outcome"), "treatment", "mediator"
)

fit <- multimedia(
    exper,
    outcome_estimator = rf_model(num.trees = 1e2)
) |>
    estimate(exper)

## ----effect_estimates---------------------------------------------------------
direct_effect(fit) |>
    effect_summary()

indirect_overall(fit) |>
    effect_summary()

## ----sensitivity_analysis-----------------------------------------------------
confound_ix <- expand.grid(mediator = 1, outcome = 1:2)
# sensitivity_curve <- sensitivity(fit, exper, confound_ix)
sensitivity_curve <- read_csv("https://go.wisc.edu/0xcyr1")
plot_sensitivity(sensitivity_curve)

## -----------------------------------------------------------------------------
perturb <- matrix(
    c(
        0, 3, 0,
        3, 0, 0,
        0, 0, 0
    ),
    nrow = 3, byrow = TRUE
)
# sensitivity_curve <- sensitivity_perturb(fit, exper, perturb)
sensitivity_curve <- read_csv("https://go.wisc.edu/75mz1b")
plot_sensitivity(sensitivity_curve, x_var = "nu")

## -----------------------------------------------------------------------------
t_mediator <- tibble(treatment = factor(rep(0:1, each = nrow(exper) / 2)))
t_outcome <- tibble(treatment = factor(rep(0:1, each = nrow(exper) / 2)))
profile <- setup_profile(fit, t_mediator, t_outcome)
xy_long[["fitted"]] <- pivot_samples(fit, profile)

## ----alteration---------------------------------------------------------------
altered_m <- nullify(fit, "T->M") |>
    estimate(exper)
altered_ty <- nullify(fit, "T->Y") |>
    estimate(exper)

fit_lm <- multimedia(
    exper,
    outcome_estimator = lm_model()
) |>
    estimate(exper)

## ----reshape_altered----------------------------------------------------------
pretty_labels <- c(
    "Original Data", "RF (Full)", "RF (T-\\->M)", "RF (T-\\->Y)", "Linear Model"
)

xy_long <- c(
    xy_long,
    list(
        altered_m = pivot_samples(altered_m, profile),
        altered_ty = pivot_samples(altered_ty, profile),
        linear = pivot_samples(fit_lm, profile)
    )
) |>
    bind_rows(.id = "fit_type") |>
    mutate(
        fit_type = case_when(
            fit_type == "linear" ~ "Linear Model",
            fit_type == "true" ~ "Original Data",
            fit_type == "fitted" ~ "RF (Full)",
            fit_type == "altered_ty" ~ "RF (T-\\->Y)",
            fit_type == "altered_m" ~ "RF (T-\\->M)"
        ),
        fit_type = factor(fit_type, levels = pretty_labels),
        outcome = case_when(
            outcome == "outcome_1" ~ "Y[1]",
            outcome == "outcome_2" ~ "Y[2]"
        )
    )

## ----visualize_long, fig.width = 8, fig.height = 3----------------------------
xy_long |>
    sample_frac(size = 0.1) |>
    ggplot(aes(mediator, col = treatment)) +
    geom_point(aes(y = value), size = 0.4, alpha = 0.9) +
    geom_rug(alpha = 0.99, linewidth = 0.1) +
    facet_grid(outcome ~ fit_type) +
    labs(x = expression("Mediator M"), y = "Outcome", col = "Treatment") +
    theme(
        strip.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12)
    )

## ----bootstraps---------------------------------------------------------------
fs <- list(direct_effect = direct_effect, indirect_overall = indirect_overall)
# bootstraps <- bootstrap(fit, exper, fs = fs)
bootstraps <- readRDS(url("https://go.wisc.edu/977l04"))

ggplot(bootstraps$direct_effect) +
    geom_histogram(aes(direct_effect)) +
    facet_wrap(~outcome)

ggplot(bootstraps$indirect_overall) +
    geom_histogram(aes(indirect_effect)) +
    facet_wrap(~outcome)

## -----------------------------------------------------------------------------
sessionInfo()

