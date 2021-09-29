## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.asp = .4,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)
library("tidyverse")
library("kableExtra")

## -----------------------------------------------------------------------------
library(marginaleffects)

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)

## -----------------------------------------------------------------------------
predictions(mod, variables = c("am", "cyl"))

## ---- echo = FALSE------------------------------------------------------------
pred <- predictions(mod, variables = c("am", "cyl")) %>%
    select(cyl, am, predicted) %>%
    pivot_wider(names_from = "am", values_from = "predicted") %>%
    rowwise() %>%
    mutate(`Marginal mean of cyl` = mean(c(`TRUE`, `FALSE`)))
row <- data.frame(x = "Marginal means of am",
                  y = mean(pred[["TRUE"]]),
                  z = mean(pred[["FALSE"]]))
colnames(row) <- colnames(pred)[1:3]
pred <- bind_rows(pred, row)
for (i in 2:ncol(pred)) {
    pred[[i]] <- sprintf("%.1f", pred[[i]])
}
pred[pred == "NA"] <- ""
kbl(pred) %>% 
    kable_styling() %>%
    add_header_above(c(" " = 1, "am" = 2, " " = 1))

## -----------------------------------------------------------------------------
marginalmeans(mod)

## -----------------------------------------------------------------------------
library(emmeans)
emmeans(mod, specs = "cyl")
emmeans(mod, specs = "am")

## -----------------------------------------------------------------------------
me <- marginalmeans(mod)

tidy(me)

glance(me)

summary(me)

## -----------------------------------------------------------------------------
library("modelsummary")

modelsummary(me,
             title = "Estimated Marginal Means",
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             group = term + group ~ model)

