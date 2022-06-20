
#' @rdname get_predict
#' @export
get_predict.mhurdle <- function(model,
                                newdata = insight::get_data(model),
                                vcov = NULL,
                                conf_level = 0.95,
                                type = "response",
                                ...) {

    out <- stats::predict(model, what = type, newdata = newdata)
    out <- data.frame(rowid = seq_len(length(out)), predicted = out)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.mhurdle <- function(model, ...) {
    out <- try(stats::vcov(model), silent = TRUE)
    if (inherits(out, "try-error")) {
        out <- tryCatch(model[["vcov"]], error = function(e) NULL)
    }
    return(out)
}