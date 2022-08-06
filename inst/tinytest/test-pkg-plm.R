source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("margins")
requiet("broom")
requiet("plm")

tol <- .001
tol_se <- .01 # BDR emergency email about tiny numerical differences

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/plm/Grunfeld.csv")
dat$X <- NULL
dat <<- pdata.frame(dat)
pool <<- plm(inv ~ value * capital, data = dat, model = "pooling")
swamy <<- plm(
inv ~ value * capital, data = dat,
model = "random", effect = "individual")
amemiya <<- plm(
inv ~ value * capital,
data = dat, model = "random", random.method = "amemiya",
effect = "twoways")
walhus <<- plm(
inv ~ value * capital,
data = dat, model = "random", random.method = "walhus",
effect = "twoways")

### marginaleffects

# pooling vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$plm_pooling
mfx <- merge(tidy(marginaleffects(pool)), stata)
expect_marginaleffects(pool, n_unique = 1)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = tol_se)



# Swamy-Arora vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$plm_sa
mfx <- merge(tidy(marginaleffects(swamy)), stata)
expect_marginaleffects(swamy)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = tol_se)

# margins
mfx <- tidy(marginaleffects(swamy))
mar <- tidy(margins(swamy))
mfx <- mfx[order(mfx$term),]
expect_equivalent(mfx$estimate, mar$estimate, tolerance = tol)
expect_equivalent(mfx$std.error, mar$std.error, tolerance = tol_se)



# no validity checks
expect_marginaleffects(amemiya)
# margins
tidy(marginaleffects(amemiya, type = "link"))
tidy(marginaleffects(amemiya, type = "response"))
mfx <- tidy(marginaleffects(amemiya))
mar <- tidy(margins(amemiya))
mfx <- mfx[order(mfx$term),]
expect_equivalent(mfx$estimate, mar$estimate, tolerance = tol)
expect_equivalent(mfx$std.error, mar$std.error, tolerance = tol_se)

expect_marginaleffects(walhus)

# margins
mfx <- tidy(marginaleffects(walhus))
mar <- tidy(margins(walhus))
mfx <- mfx[order(mfx$term),]
expect_equivalent(mfx$estimate, mar$estimate, tolerance = tol)
expect_equivalent(mfx$std.error, mar$std.error, tolerance = tol_se)



# # commented out because the dev version of {plm} now has a fully-working predict method
# # within error
# # within model are not supported by `predict.plm`
# stata <- readRDS(testing_path("stata/stata.rds"))$plm_within
# mod <- plm(inv ~ value * capital, data = dat, model = "within", effect = "twoways")
# expect_error(marginaleffects(mod), pattern = "Unable")



### predictions

# predictions: pooling no validity
pred1 <- predictions(pool)
pred2 <- predictions(pool, newdata = head(dat))
expect_predictions(pred1, n_row = nrow(dat))
expect_predictions(pred2, n_row = 6)
