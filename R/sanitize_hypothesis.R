sanitize_hypothesis <- function(hypothesis, ...) {

    if (isTRUE(checkmate::check_formula(hypothesis))) {
        hypothesis <- sanitize_hypothesis_formula(hypothesis)
    }

    checkmate::assert(
        checkmate::check_character(hypothesis, pattern = "="),
        checkmate::check_choice(hypothesis, choices = c("pairwise", "reference", "sequential", "meandev", "meanotherdev", "revpairwise", "revreference", "revsequential")),
        checkmate::check_numeric(hypothesis),
        checkmate::check_matrix(hypothesis),
        checkmate::check_function(hypothesis),
        checkmate::check_null(hypothesis))

    hnull <- 0

    if (isTRUE(checkmate::check_character(hypothesis, pattern = "="))) {
        out <- paste(gsub("=", "-(", hypothesis), ")")
        attr(out, "label") <- hypothesis
        hypothesis <- out

    } else if (isTRUE(checkmate::check_matrix(hypothesis))) {
        attr(hypothesis, "label") <- colnames(hypothesis)

    }  else if (isTRUE(checkmate::check_numeric(hypothesis, len = 1))) {
        hnull <- hypothesis
        hypothesis <- NULL
    }

    out <- list(
        "hypothesis" = hypothesis,
        "hypothesis_null" = hnull
    )

    return(out)

}
