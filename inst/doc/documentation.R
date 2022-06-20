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
url <- "https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/main/data-raw/supported_models.csv"
dat <- read.csv(url)
n_support <- nrow(dat)

