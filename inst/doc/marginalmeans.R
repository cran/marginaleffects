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
mm <- marginalmeans(mod)

tidy(mm)

glance(mm)

summary(mm)

## -----------------------------------------------------------------------------
library("modelsummary")

modelsummary(mm,
             title = "Estimated Marginal Means",
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             group = term + value ~ model)

## -----------------------------------------------------------------------------
library(nnet)
library(marginaleffects)

set.seed(1839)
n <- 1200
x <- factor(sample(letters[1:3], n, TRUE))
y <- vector(length = n)
y[x == "a"] <- sample(letters[4:6], sum(x == "a"), TRUE)
y[x == "b"] <- sample(letters[4:6], sum(x == "b"), TRUE, c(1 / 4, 2 / 4, 1 / 4))
y[x == "c"] <- sample(letters[4:6], sum(x == "c"), TRUE, c(1 / 5, 3 / 5, 2 / 5))

dat <- data.frame(x = x, y = factor(y))
tmp <- as.data.frame(replicate(20, factor(sample(letters[7:9], n, TRUE))))
dat <- cbind(dat, tmp)
void <- capture.output({
    mod <- multinom(y ~ ., dat)
})

## ---- error = TRUE------------------------------------------------------------
marginalmeans(mod, type = "probs")

## ---- eval = FALSE------------------------------------------------------------
#  marginalmeans(mod,
#                type = "probs",
#                variables = c("x", "V1"),
#                variables_grid = paste0("V", 2:3))

