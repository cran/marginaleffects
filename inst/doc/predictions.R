## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.asp = .4,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

library(marginaleffects)
library(patchwork)
library(ggplot2)

theme_set(theme_minimal())

## -----------------------------------------------------------------------------
library(marginaleffects)

mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)

pred <- predictions(mod)

head(pred)

## -----------------------------------------------------------------------------
predictions(mod, variables = c("cyl", "hp"))

## ---- message = FALSE---------------------------------------------------------
library(kableExtra)
library(tidyverse)

predictions(mod, variables = c("cyl", "hp")) %>%
    select(hp, cyl, predicted) %>%
    pivot_wider(values_from = predicted, names_from = cyl) %>%
    kbl(caption = "A table of Adjusted Predictions") %>%
    kable_styling() %>%
    add_header_above(header = c(" " = 1, "cyl" = 3))

## -----------------------------------------------------------------------------
datagrid(cyl = c(4, 6, 8), model = mod)

## -----------------------------------------------------------------------------
predictions(mod, newdata = datagrid())

predictions(mod, newdata = datagrid(cyl = c(4, 6, 8)))

## -----------------------------------------------------------------------------
mod <- glm(vs ~ hp + am, data = mtcars, family = binomial)

nd <- datagrid(model = mod, am = 0:1, grid.type = "counterfactual")

dim(nd)

## ---- fig.asp = 1-------------------------------------------------------------
pred <- predictions(mod, newdata = datagrid(am = 0:1, grid.type = "counterfactual")) %>%
    select(am, predicted, rowid_original) %>%
    pivot_wider(id_cols = rowid_original, 
                names_from = am,
                values_from = predicted)

ggplot(pred, aes(x = `0`, y = `1`)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "Predicted Pr(vs=1), when am = 0",
         y = "Predicted Pr(vs=1), when am = 1")

## ---- message = FALSE---------------------------------------------------------
library(tidyverse)
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2movies/movies.csv") %>%
    mutate(style = case_when(Action == 1 ~ "Action",
                             Comedy == 1 ~ "Comedy",
                             Drama == 1 ~ "Drama",
                             TRUE ~ "Other"),
           style = factor(style),
           certified_fresh = rating >= 8) %>%
    filter(length < 240)

mod <- glm(certified_fresh ~ length * style, data = dat, family = binomial)

## -----------------------------------------------------------------------------
mod <- glm(certified_fresh ~ length, data = dat, family = binomial)

plot_cap(mod, condition = "length")

## -----------------------------------------------------------------------------
mod <- glm(certified_fresh ~ length * style, data = dat, family = binomial)

plot_cap(mod, condition = c("length", "style"))

## -----------------------------------------------------------------------------
predictions(mod,
            type = c("response", "link"),
            newdata = datagrid(length = 90:120,
                               style = c("Action", "Comedy"))) %>%
    ggplot(aes(length, predicted, color = style))  +
    geom_line() +
    facet_wrap(~type, scales = "free_y")

## -----------------------------------------------------------------------------
mod <- glm(am ~ mpg, family = binomial, data = mtcars)
pred <- predictions(mod, type = c("response", "link"))
head(pred)

## -----------------------------------------------------------------------------
plot_cap(mod, condition = "mpg", type = "response")

## -----------------------------------------------------------------------------
plot_cap(mod, condition = "mpg", type = "link")

