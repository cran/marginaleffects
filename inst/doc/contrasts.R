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

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)

## -----------------------------------------------------------------------------
mfx <- marginaleffects(mod)
summary(mfx)

## -----------------------------------------------------------------------------
library(emmeans)
emm <- emmeans(mod, specs = "cyl")
contrast(emm, method = "revpairwise")

emm <- emmeans(mod, specs = "am")
contrast(emm, method = "revpairwise")

## -----------------------------------------------------------------------------
mod_int <- lm(mpg ~ am * factor(cyl), tmp)

## -----------------------------------------------------------------------------
marginaleffects(mod_int, newdata = datagrid(cyl = tmp$cyl), variables = "am")

## -----------------------------------------------------------------------------
emm <- emmeans(mod_int, specs = "am", by = "cyl")
contrast(emm, method = "revpairwise")

