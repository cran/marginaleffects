## ---- include = FALSE---------------------------------------------------------
# this vignette is in .Rbuildignore because lme4 is not available on old CRAN
# test machines.

knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(marginaleffects)
mod <- lm(mpg ~ hp + wt, data = mtcars)

marginaleffects(mod) |> summary()

marginaleffects(mod, slope = "eyex") |> summary()

marginaleffects(mod, slope = "eydx") |> summary()

marginaleffects(mod, slope = "dyex") |> summary()

