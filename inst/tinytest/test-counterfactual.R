source("helpers.R", local = TRUE)

# old bug: counterfactual with a single regressor
mod <- lm(mpg ~ hp + drat + wt, mtcars)
x <- datagrid(model = mod, hp = c(100, 110), grid.type = "counterfactual")
expect_equivalent(nrow(x), 64)
mod <- lm(mpg ~ hp, mtcars)
x <- datagrid(model = mod, hp = c(100, 110), grid.type = "counterfactual")
expect_equivalent(nrow(x), 64)



# marginal effects does not overwrite counterfactual rowid
mod <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
mfx <- marginaleffects(mod, newdata = datagrid(cyl = c(4, 6, 8), grid.type = "counterfactual"))
expect_true(all(mfx$rowidcf %in% 1:32))
expect_true(all(mfx$rowid %in% 1:96))



# alternative syntaxes
mod <- lm(mpg ~ hp + drat + wt, mtcars)
nd1 <- datagrid(wt = 3, hp = c(100, 110), model = mod)
nd2 <- datagrid(wt = 3, hp = c(100, 110), newdata = mtcars)
x <- marginaleffects(mod, newdata = datagrid(wt = 3, hp = c(100, 110)))
y <- marginaleffects(mod, newdata = nd1)
z <- marginaleffects(mod, newdata = nd2)
z <- z[, colnames(x), drop = FALSE]
expect_true(all(x == y))
expect_true(all(x == z))



# size
mod <- lm(mpg ~ hp + drat + wt, mtcars)
expect_equivalent(nrow(datagrid(wt = 2:3, newdata = mtcars, grid.type = "counterfactual")), nrow(mtcars) * 2)
expect_equivalent(nrow(datagrid(wt = 2:3, newdata = mtcars)), 2)
expect_equivalent(nrow(datagrid(wt = 2:3, model = mod, grid.type = "counterfactual")), nrow(mtcars) * 2)
expect_equivalent(nrow(datagrid(wt = 2:3, model = mod)), 2)



# warning on bad `at` entry
expect_warning(datagrid(newdata = mtcars, at = list("blah" = 0:1), grid.type = "counterfactual"))
expect_warning(datagrid(newdata = mtcars, at = list("blah" = 0:1)))



# datagrid(): factor, logical, automatic variable
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$gear <- as.factor(tmp$gear)
mod <- lm(mpg ~ hp * wt + am + gear, data = tmp)
res <- datagrid(
    model = mod,
    hp = c(100, 110),
    gear = c(3, 4),
    am = TRUE,
    grid.type = "counterfactual")
expect_inherits(res, "data.frame")
expect_equivalent(dim(res), c(128, 6))



# datagrid(): factor, logical, numeric
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$gear <- as.factor(tmp$gear)
res <- datagrid(newdata = tmp)
expect_inherits(res, "data.frame")
expect_equivalent(dim(res), c(1, 11))
expect_equivalent(sum(sapply(res, is.logical)), 1)
expect_equivalent(sum(sapply(res, is.factor)), 1)
expect_equivalent(sum(sapply(res, is.numeric)), 9)



# typical number of rows
mod <- lm(mpg ~ hp * wt, data = mtcars)
nd <- datagrid(model = mod, hp = c(100, 110))
expect_equivalent(nrow(marginaleffects(mod, newdata = nd)), 4)
