#' @importFrom generics tidy
#' @export
generics::tidy


#' @importFrom generics glance
#' @export
generics::glance


#' tidy helper
#' 
#' @noRd
#' @export
tidy.comparisons <- function(x, ...) {
    insight::check_if_installed("tibble")
    out <- tibble::as_tibble(x)
    if (!"term" %in% names(out)) {
        lab <- seq_len(nrow(out))
        if ("group" %in% colnames(out) || is.character(attr(x, "by"))) {
            tmp <- c("group", attr(x, "by"))
            tmp <- Filter(function(j) j %in% colnames(x), tmp)
            if (length(tmp) > 0) {
                tmp <- do.call(paste, out[, tmp])
                if (anyDuplicated(tmp)) {
                    tmp <- paste(seq_len(nrow(out)), tmp)
                }
                lab <- tmp
            }
        }
        out[["term"]] <- lab
    }
    return(out)
}


#' tidy helper
#' 
#' @noRd
#' @export
tidy.slopes <- tidy.comparisons


#' tidy helper
#' 
#' @noRd
#' @export
tidy.predictions <- tidy.comparisons


#' tidy helper
#' 
#' @noRd
#' @export
tidy.hypotheses <- tidy.comparisons


#' tidy helper
#' 
#' @noRd
#' @export
tidy.marginalmeans <- function(x, ...) {
    insight::check_if_installed("tibble")
    tibble::as_tibble(x)
}


#' @noRd
#' @export
glance.slopes <- function(x, ...) {
    insight::check_if_installed("insight")
    insight::check_if_installed("modelsummary")
    model <- tryCatch(attr(x, "model"), error = function(e) NULL)
    if (is.null(model) || isTRUE(checkmate::check_string(model))) {
        model <- tryCatch(attr(x, "call")[["model"]], error = function(e) NULL)
    }
    gl <- suppressMessages(suppressWarnings(try(
        modelsummary::get_gof(model, ...), silent = TRUE)))
    if (inherits(gl, "data.frame")) {
        out <- data.frame(gl)
    } else {
        out <- NULL
    }
    vcov.type <- attr(x, "vcov.type")
    if (is.null(out) && !is.null(vcov.type)) {
        out <- data.frame("vcov.type" = vcov.type)
    } else if (!is.null(out)) {
        out[["vcov.type"]] <- vcov.type
    }
    out <- tibble::as_tibble(out)
    return(out)
}


#' @noRd
#' @export
glance.predictions <- glance.slopes


#' @noRd
#' @export
glance.comparisons <- glance.slopes


#' @noRd
#' @export
glance.hypotheses <- glance.slopes


#' @noRd
#' @export
glance.marginalmeans <- glance.slopes