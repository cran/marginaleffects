## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
library("data.table")
library("kableExtra")

dat <- "https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/main/data-raw/supported_models.csv"
#dat <- "data-raw/supported_models.csv"
supported_models <- fread(dat, colClasses = rep("character", 10))

tmp <- supported_models
for (i in nrow(tmp):2) {
    if (tmp$Package[i] == tmp$Package[i - 1]) {
        tmp$Package[i] <- ""
    }
}

colnames(tmp) <- c("Package",
                   "Function",
                   "dY/dX",
                   "SE",
                   "dY/dX ",
                   "SE ",
                   "dY/dX  ",
                   "SE  ",
                   "dY/dX   ",
                   "SE   ")

# remove "supported" columns which feel useless since everything is supported
tmp[[3]] <- tmp[[4]] <- NULL

# colors & icons
f <- function(x) fcase(x == TRUE, "green", x == FALSE, "red", default = "grey")
idx <- lapply(tmp, f)

for (i in 3:ncol(tmp)) {
    tmp[[i]] <- as.character(tmp[[i]])
    tmp[[i]] <- fcase(tmp[[i]] == "TRUE", "✓",
                      tmp[[i]] == "FALSE", "✖",
                      tmp[[i]] == "U", "U",
                      default = "")
}

## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
# kableExtra
tab <- kbl(tmp, align = c("l", "l", rep("c", 6)), full_width = FALSE)
tab <- kable_styling(tab)
for (i in 3:ncol(tmp)) {
    tab <- column_spec(tab, column = i, color = idx[[i]])
}
tab <- add_header_above(tab, c("Supported by marginaleffects" = 2, "Stata" = 2, "margins" = 2, "emtrends" = 2))
tab <- add_header_above(tab, c(" " = 2, "Numerical equivalence" = 6))
tab

