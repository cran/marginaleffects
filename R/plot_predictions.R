#' Plot Conditional or Marginal Predictions
#'
#' @description
#' Plot predictions on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).
#'
#' The `by` argument is used to plot marginal predictions, that is, predictions made on the original data, but averaged by subgroups. This is analogous to using the `by` argument in the `predictions()` function.
#'
#' The `condition` argument is used to plot conditional predictions, that is, predictions made on a user-specified grid. This is analogous to using the `newdata` argument and `datagrid()` function in a `predictions()` call. Unspecified variables are held at their mean or mode.
#' 
#' See the "Plots" vignette and website for tutorials and information on how to customize plots:
#'
#' * https://vincentarelbundock.github.io/marginaleffects/articles/plot.html
#' * https://vincentarelbundock.github.io/marginaleffects
#' 
#' @param condition Conditional predictions
#' + Character vector (max length 3): Names of the predictors to display.
#' + Named list (max length 3): List names correspond to predictors. List elements can be:
#'   - Numeric vector
#'   - Function which returns a numeric vector or a set of unique categorical values 
#'   - Shortcut strings for common reference values: "minmax", "quartile", "threenum"
#' + 1: x-axis. 2: color/shape. 3: facets.
#' + Numeric variables in positions 2 and 3 are summarized by Tukey's five numbers `?stats::fivenum`
#' @param by Marginal predictions
#' + Character vector (max length 3): Names of the categorical predictors to marginalize across.
#' + 1: x-axis. 2: color. 3: facets.
#' @param points Number between 0 and 1 which controls the transparency of raw data points. 0 (default) does not display any points.
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot_slopes
#' @inheritParams predictions
#' @return A `ggplot2` object or data frame (if `draw=FALSE`)
#' @export
#' @examples
#' mod <- lm(mpg ~ hp + wt, data = mtcars)
#' plot_predictions(mod, condition = "wt")
#'
#' mod <- lm(mpg ~ hp * wt * am, data = mtcars)
#' plot_predictions(mod, condition = c("hp", "wt"))
#'
#' plot_predictions(mod, condition = list("hp", wt = "threenum"))
#' 
#' plot_predictions(mod, condition = list("hp", wt = range))
#'
plot_predictions <- function(model,
                             condition = NULL,
                             by = NULL,
                             type = NULL,
                             vcov = NULL,
                             conf_level = 0.95,
                             transform_post = NULL,
                             points = 0,
                             rug = FALSE,
                             gray = FALSE,
                             draw = TRUE,
                             ...) {

    dots <- list(...)
    
    if ("variables" %in% names(dots)) {
        insight::format_error("The `variables` argument is not supported by this function.")
    }
    if ("effect" %in% names(dots)) {
        insight::format_error("The `effect` argument is not supported by this function.")
    }

    checkmate::assert_number(points, lower = 0, upper = 1)

    # sanity check
    checkmate::assert_character(by, null.ok = TRUE, max.len = 3, min.len = 1, names = "unnamed")
    if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
        msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
        insight::format_error(msg)
    }
    if (is.null(by)) by <- FALSE

    # conditional
    if (!is.null(condition)) {
        modeldata <- get_modeldata(model, additional_variables = names(condition$condition))
        condition <- sanitize_condition(model, condition, variables = NULL, modeldata = modeldata)
        v_x <- condition$condition1
        v_color <- condition$condition2
        v_facet <- condition$condition3
        datplot <- predictions(
            model,
            newdata = condition$newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            transform_post = transform_post,
            modeldata = modeldata,
            ...)
    }

    # marginal
    if (!isFALSE(by)) { # switched from NULL above
        condition <- NULL
        modeldata <- get_modeldata(model, additional_variables = by)
        datplot <- predictions(
            model,
            by = by,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            wts = NULL,
            transform_post = transform_post,
            modeldata = modeldata,
            ...)
        v_x <- by[[1]]
        v_color <- hush(by[[2]])
        v_facet <- hush(by[[3]])
    }

    dv <- unlist(insight::find_response(model, combine = TRUE), use.names = FALSE)[1]

    datplot <- plot_preprocess(datplot, v_x = v_x, v_color = v_color, v_facet = v_facet, condition = condition, modeldata = modeldata)

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        out <- as.data.frame(datplot)
        attr(out, "posterior_draws") <- attr(datplot, "posterior_draws")
        return(out)
    }

    # ggplot2
    insight::check_if_installed("ggplot2")
    p <- plot_build(datplot,
        v_x = v_x,
        v_color = v_color,
        v_facet = v_facet,
        points = points,
        modeldata = modeldata,
        dv = dv,
        rug = rug,
        gray = gray)
    
    p <- p + ggplot2::labs(
        x = v_x,
        y = dv, 
        color = v_color,
        fill = v_color,
        linetype = v_color)

    # attach model data for each of use
    attr(p, "modeldata") <- modeldata

    return(p)
}




#' `plot_predictions()` is an alias to `plot_predictions()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cap <- plot_predictions