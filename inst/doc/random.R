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

## ----libraries----------------------------------------------------------------
library(multimedia)
library(ggplot2)
library(ggraph)

## ----example_data-------------------------------------------------------------
demo_joy()

## ----setup--------------------------------------------------------------------
exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
exper

## ----estimate-----------------------------------------------------------------
model <- multimedia(exper) |>
    estimate(exper)

model

## ----graph--------------------------------------------------------------------
ggraph(edges(model)) +
    geom_edge_link(arrow = arrow()) +
    geom_node_label(aes(label = name, fill = node_type))

## ----operations---------------------------------------------------------------
sample(model)
predict(model)

## ----new_treat----------------------------------------------------------------
t_mediator <- factor(c("Treatment", rep("Control", 3)))
t_outcome <- factor(rep("Control", 4), levels = c("Treatment", "Control"))

profile <- setup_profile(model, t_mediator, t_outcome)
sample(model, profile = profile)
predict(model, profile = profile)

setup_profile(model, t_mediator, t_outcome)

## ----contrast-----------------------------------------------------------------
profile_control <- setup_profile(model, t_outcome, t_outcome)
contrast_predictions(model, profile, profile_control)
contrast_samples(model, profile, profile_control)

## ----effects------------------------------------------------------------------
direct_effect(model, exper)
indirect_overall(model, exper)
indirect_pathwise(model, exper)

## ----glmnet-------------------------------------------------------------------
model <- multimedia(exper, glmnet_model(lambda = .1)) |>
    estimate(exper)

direct_effect(model, exper)
indirect_overall(model, exper)
indirect_pathwise(model, exper)

## ----bootstrap----------------------------------------------------------------
bootstrap(model, exper, c(direct_effect = direct_effect))$direct_effect |>
    head(10)

## ----false_discovery----------------------------------------------------------
contrast <- null_contrast(model, exper, "M->Y", indirect_pathwise)
fdr <- fdr_summary(contrast, "indirect_pathwise", 0.05)
fdr

## -----------------------------------------------------------------------------
sessionInfo()

