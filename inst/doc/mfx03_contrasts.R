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
library(magrittr)

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)

## -----------------------------------------------------------------------------
mfx <- marginaleffects(mod)
summary(mfx)

## -----------------------------------------------------------------------------
comparisons(mod, contrast_factor = "sequential") %>% tidy()
comparisons(mod, contrast_factor = "pairwise") %>% tidy()
comparisons(mod, contrast_factor = "reference") %>% tidy()

## -----------------------------------------------------------------------------
library(emmeans)
emm <- emmeans(mod, specs = "cyl")
contrast(emm, method = "revpairwise")

emm <- emmeans(mod, specs = "am")
contrast(emm, method = "revpairwise")

## -----------------------------------------------------------------------------
mod_logit <- glm(am ~ factor(gear), data = mtcars, family = binomial)

comparisons(mod_logit) %>% tidy()

comparisons(mod_logit, type = "link") %>% tidy()

## -----------------------------------------------------------------------------
mod <- lm(mpg ~ hp, data = mtcars)

comparisons(mod) %>% tidy()

comparisons(mod, contrast_numeric = 5) %>% tidy()

## -----------------------------------------------------------------------------
comparisons(mod, contrast_numeric = c(90, 110)) %>% tidy()

## -----------------------------------------------------------------------------
comparisons(mod, contrast_numeric = "iqr") %>% tidy()

comparisons(mod, contrast_numeric = "sd") %>% tidy()

comparisons(mod, contrast_numeric = "2sd") %>% tidy()

comparisons(mod, contrast_numeric = "minmax") %>% tidy()

## -----------------------------------------------------------------------------
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)

## -----------------------------------------------------------------------------
cmp <- comparisons(mod, variables = c("cyl", "am"))
summary(cmp)

## ---- warning = FALSE---------------------------------------------------------
library(marginaleffects)
mod <- glm(vs ~ factor(gear) + mpg, family = binomial, data = mtcars)
cmp <- comparisons(mod, variables = "mpg")
nrow(cmp)

## -----------------------------------------------------------------------------
summary(cmp)

## -----------------------------------------------------------------------------
mean(cmp$comparison)

## -----------------------------------------------------------------------------
library(ggplot2)

cmp <- comparisons(mod, variables = "gear")

ggplot(cmp, aes(comparison)) +
    geom_histogram(bins = 30) +
    facet_wrap(~contrast, scale = "free_x") +
    labs(x = "Distribution of unit-level contrasts")

## -----------------------------------------------------------------------------
comparisons(mod, variables = "mpg", newdata = "mean")

## -----------------------------------------------------------------------------
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
mod <- lm(bill_length_mm ~ species * sex + island + body_mass_g, data = dat)

cmp <- comparisons(
    mod,
    newdata = "marginalmeans",
    variables = c("species", "island"))
summary(cmp)

## -----------------------------------------------------------------------------
emm <- emmeans(
    mod,
    specs = c("species", "island"))
contrast(emm, method = "trt.vs.ctrl1")

