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

## ---- echo=FALSE--------------------------------------------------------------
x <- seq(-4, 4, .01)
y <- -x^2 
dat <- data.frame(x, y)
ggplot(dat, aes(x, y)) + 
    geom_line() +
    labs(y = "Response") +
    xlim(c(-4, 4)) +
    ylim(c(-7, 0.5))

## ---- echo=FALSE--------------------------------------------------------------
p1 <- ggplot(dat, aes(x, y)) + 
      xlim(c(-4, 4)) +
      ylim(c(-7, 0.5)) +
      labs(y = "Response") +
      geom_abline(slope = 4, intercept = 4, color = "orange", linetype = "dashed") +
      geom_abline(slope = 0, intercept = 0, color = "orange", linetype = "dashed") +
      geom_abline(slope = -4, intercept = 4, color = "orange", linetype = "dashed") +
      geom_line() +
      annotate("point", x = -2, y = -4, colour = "orange") +
      annotate("point", x = 0, y = 0, colour = "orange") +
      annotate("point", x = 2, y = -4, colour = "orange")
p1

## -----------------------------------------------------------------------------
library(marginaleffects)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)

mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species,
           data = dat, family = binomial)

## -----------------------------------------------------------------------------
mfx <- marginaleffects(mod)
head(mfx)

## -----------------------------------------------------------------------------
summary(mfx)

## -----------------------------------------------------------------------------
tidy(mfx)

glance(mfx)

## -----------------------------------------------------------------------------
datagrid(flipper_length_mm = 180,
         species = c("Adelie", "Gentoo"),
         model = mod)

## -----------------------------------------------------------------------------
marginaleffects(mod,
                newdata = datagrid(flipper_length_mm = 180,
                                   species = c("Adelie", "Gentoo")))

## -----------------------------------------------------------------------------
nd <- datagrid(flipper_length_mm = c(160, 180), model = mod, grid.type = "counterfactual")

## -----------------------------------------------------------------------------
nd[nd$rowid %in% 1:3,]

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)

marginaleffects(mod, newdata = nd) %>%
    group_by(term) %>%
    summarize(across(dydx:std.error, median))

## ---- out.width = "60%"-------------------------------------------------------
mod <- lm(mpg ~ hp * wt + drat, data = mtcars)

plot_cme(mod, effect = "hp", condition = "wt")

## ---- message = FALSE---------------------------------------------------------
library(tidyverse)
N <- 1e5
quad <- data.frame(x = rnorm(N))
quad$y <- 1 + 1 * quad$x + 2 * quad$x^2 + rnorm(N)
mod <- lm(y ~ x + I(x^2), quad)

marginaleffects(mod, newdata = datagrid(x = -2:2))  %>%
    mutate(truth = 1 + 4 * x) %>%
    select(dydx, truth)

## -----------------------------------------------------------------------------
plot_cme(mod, effect = "x", condition = "x")

## -----------------------------------------------------------------------------
mod <- glm(am ~ mpg, family = binomial, data = mtcars)
mfx <- marginaleffects(mod, type = c("response", "link"))
summary(mfx)

## ---- echo = FALSE------------------------------------------------------------
options(modelsummary_factory_default = "markdown")


## -----------------------------------------------------------------------------
library(modelsummary)
library(marginaleffects)

# fit models and store them in a named list
mod <- list(
    "Short" = glm(large_penguin ~ flipper_length_mm, data = dat, family = binomial),
    "Long" = glm(large_penguin ~ flipper_length_mm + bill_length_mm, data = dat, family = binomial))

# apply the `marginaleffects` function to all the models using `lapply`
mfx <- lapply(mod, marginaleffects)

modelsummary(mfx)

## -----------------------------------------------------------------------------
modelplot(mfx) + ggplot2::xlab("Average Marginal Effects with 95% Confidence Intervals")

## -----------------------------------------------------------------------------
mod <- list(
    "Logit" = glm(large_penguin ~ flipper_length_mm + species, data = dat, family = binomial),
    "OLS" = lm(body_mass_g ~ flipper_length_mm + bill_length_mm + species, data = dat))

mfx <- lapply(mod, marginaleffects)

modelsummary(mfx, group = term + contrast ~ model)

