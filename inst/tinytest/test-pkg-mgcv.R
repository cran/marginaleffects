source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")

requiet("mgcv")
requiet("emmeans")
requiet("broom")
requiet("dplyr")
requiet("tsModel")


# marginaleffects vs. emtrends
set.seed(2)
void <- capture.output(dat <- gamSim(1, n = 400, dist = "normal", scale = 2))
void <- capture.output(dat2 <- gamSim(1, n = 2000, dist = "poisson", scale = .1))
m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
m2 <- mgcv::gam(y ~ te(x0, x1, k = 7) + s(x2) + s(x3), data = dat, method = "REML")
m3 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) + ti(x1, x2, k = 6), data = dat, method = "REML")
m4 <- mgcv::gam(y ~ s(x0, x1, k = 40) + s(x2) + s(x3), data = dat, method = "REML")
m5 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML", select = TRUE)
m6 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), sp = c(0.01, -1, -1, -1), data = dat)
m7 <- mgcv::gam(y ~ s(x0, sp = .01) + s(x1) + s(x2) + s(x3), data = dat)
m8 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), min.sp = c(0.001, 0.01, 0, 10), data = dat)
m9 <- mgcv::gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
      s(x3, bs = "cr"), family = poisson, data = dat2, method = "REML")
expect_marginaleffects(m1)
expect_marginaleffects(m2)
expect_marginaleffects(m3)
expect_marginaleffects(m4)
expect_marginaleffects(m5)
expect_marginaleffects(m6)
expect_marginaleffects(m7)
expect_marginaleffects(m8)
expect_marginaleffects(m9)


# emtrends
mfx <- marginaleffects(m1, variables = "x1", newdata = datagrid(x1 = 0, x2 = 0, x3 = 0), type = "link")
em <- emtrends(m1, ~x1, "x1", at = list(x1 = 0, x2 = 0, x3 = 0))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$x1.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .0001)


# predictions: no validity
set.seed(2)
void <- capture.output(dat <- gamSim(1, n = 400, dist = "normal", scale = 2))
mod <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
pred1 <- predictions(mod)
pred2 <- predictions(mod, newdata = head(dat))
expect_predictions(pred1, n_row = nrow(dat))
expect_predictions(pred2, n_row = 6)


# Issue #364: predictions confidence intervals for binomial models
if (packageVersion("insight") > "0.17.1.6") {
    void <- capture.output(
        dat <- suppressMessages(gamSim(1, n = 400, dist = "binary", scale = .33)))
    m <- bam(
      y ~ s(x0) + s(x1) + s(x2) + s(x3),
      family = binomial,
      data = dat,
      method = "REML"
    )
    p <- predictions(m)
    expect_true("conf.low" %in% colnames(p))
    expect_true("conf.high" %in% colnames(p))
}


# Issue #363: matrix column in predictors
if (packageVersion("insight") > "0.17.1.6") {
    test1 <- function(x,z,sx=0.3,sz=0.4) { 
      x <- x*20
      (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
                     0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
    }
    n <- 500
    x <- runif(n)/20;z <- runif(n);
    f <- test1(x,z)
    y <- f + rnorm(n)*0.2
    df <- tibble(y, x, z) %>% 
      mutate(x_lags = tsModel::Lag(x, 0:10),
             L = matrix(0:10, nrow = 1))
    b <- mgcv::gam(y ~ s(z) + te(x_lags, L), data = df)
    mfx <- suppressWarnings(marginaleffects(b))
    cmp <- suppressWarnings(comparisons(b))
    pre <- predictions(b)
    expect_inherits(pre, "predictions")
    expect_inherits(mfx, "marginaleffects")
    expect_inherits(cmp, "comparisons")
    # only one regressor since others are matrix columns
    expect_true(all(mfx$term == "z"))
    expect_true(all(cmp$term == "z"))

    expect_error(suppressWarnings(marginaleffects(b, variables = "L")), pattern = "no valid")
    expect_error(suppressWarnings(comparisons(b, variables = "L")), pattern = "no valid")
    expect_warning(plot_cap(b, condition = "z"), pattern = "Matrix columns")
    expect_warning(plot_cme(b, effect = "L", condition = "z"), pattern = "Matrix columns")
}


# Issue #365: exclude argument changes predictions
if (packageVersion("insight") > "0.17.1.6") {
    void <- capture.output(
        dat <- gamSim(1,n=400,dist="normal",scale=2)
    )
    b <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
    p1 <- predictions(b)
    p2 <- predictions(b, exclude = "s(x3)")
    expect_true(all(p1$predicted != p2$predicted))


    # exclude a smooth
    requiet("itsadug")
    set.seed(1024)
    data(simdat)
    simdat$Subject <- as.factor(simdat$Subject)
    model <- bam(Y ~ Group + s(Time, by = Group) + s(Subject, bs = "re"), data = simdat)
    nd <- datagrid(
        model = model,
        Subject = "a01",
        Group = "Adults")

    expect_equivalent(
        predictions(model, newdata = nd)$predicted,
        predict(model, newdata = nd)[1])

    expect_equivalent(
        predictions(model, newdata = nd, exclude = "s(Subject)")$predicted,
        predict(model, newdata = nd, exclude = "s(Subject)")[1])

    mfx <- marginaleffects(model, newdata = "mean", variables = "Time", type = "link")
    emt <- suppressMessages(data.frame(
        emtrends(model, ~Time, "Time", at = list(Time = 1000, Subject = "a01", Group = "Adults"))))
    expect_equivalent(mfx$dydx, emt$Time.trend, tolerance = 1e-2)
    expect_equivalent(mfx$std.error, emt$SE, tolerance = 1e-3)
}