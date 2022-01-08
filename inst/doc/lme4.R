## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

library(ggplot2)

theme_clean <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(size = rel(1), hjust = 0),
          strip.background = element_blank(),
          legend.position = "bottom")
}
ggplot2::theme_set(theme_clean())

## ---- message = FALSE, warning = FALSE----------------------------------------
library(lme4)
library(tidyverse)
library(patchwork)
library(marginaleffects)

# unconditional linear growth model
fit1 <- lmer(
  weight ~ 1 + Time + (1 + Time | Chick),
  data = ChickWeight)

# conditional quadratic growth model
fit2 <- lmer(
  weight ~ 1 + Time + I(Time^2) + Diet + Time:Diet + I(Time^2):Diet + (1 + Time + I(Time^2) | Chick),
  data = ChickWeight)

## -----------------------------------------------------------------------------
pred1 <- predictions(fit1,
                     newdata = datagrid(Chick = ChickWeight$Chick,
                                        Time = 0:21))

p1 <- ggplot(pred1, aes(Time, predicted, level = Chick)) +
      geom_line() +
      labs(y = "Predicted weight", x = "Time", title = "Linear growth model")

pred2 <- predictions(fit2,
                     newdata = datagrid(Chick = ChickWeight$Chick,
                                        Time = 0:21))

p2 <- ggplot(pred2, aes(Time, predicted, level = Chick)) +
      geom_line() +
      labs(y = "Predicted weight", x = "Time", title = "Quadratic growth model")

p1 + p2

## -----------------------------------------------------------------------------
pred <- predictions(fit2)

ggplot(pred, aes(Time, predicted, level = Chick)) +
    geom_line() +
    ylab("Predicted Weight") +
    facet_wrap(~ Diet, labeller = label_both)

## -----------------------------------------------------------------------------
pred <- predictions(
    fit2,
    newdata = datagrid(Chick = NA,
                       Diet = 1:4,
                       Time = 0:21),
    include_random = FALSE)

ggplot(pred, aes(x = Time, y = predicted, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .1, fill = "red") +
    geom_line() +
    facet_wrap(~ Diet, labeller = label_both) +
    labs(title = "Population-level trajectories")

