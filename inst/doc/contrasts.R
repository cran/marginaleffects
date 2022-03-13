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
mod_int <- lm(mpg ~ am * factor(cyl), tmp)

## -----------------------------------------------------------------------------
comparisons(mod_int, newdata = datagrid(cyl = tmp$cyl), variables = "am")

## -----------------------------------------------------------------------------
emm <- emmeans(mod_int, specs = "am", by = "cyl")
contrast(emm, method = "revpairwise")

