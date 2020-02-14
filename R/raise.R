#' Raise a condition
#'
#' A generic function for raising any condition, including messages, warnings,
#' and errors. Conditions of other classes should define a method for this
#' generic.
#'
#' @param condition A [condition()] to raise
#'
#' @export
raise <- function(condition){
    attrs <- attributes(condition)
    if ("capture_call" %in% names(attrs)) {
        capture_call <- isTRUE(attrs[["capture_call"]])
    } else {
        capture_call <- FALSE
    }

    if ("call" %in% names(condition)) {
        # if a call is already included in the condition, do not capture
        capture_call <- capture_call && is.null(condition[["call"]])
    }

    if (capture_call) {
        condition[["call"]] <- match.call(
            definition = sys.function(sys.parent(1L)),
            call = sys.call(sys.parent(1L))
        )
    }

    # so altered condition gets passed to method
    ( function(condition) UseMethod("raise") )(condition)
}

#' @export
raise.default <- function(condition){
    stop("`raise` method for class `", class(condition), "` is not defined")
}

#' @export
raise.message <- function(condition){
    withRestarts(
        message(condition),
        muffle = function(handler, cond) handler(cond)
    )
}

#' @export
raise.warning <- function(condition){
    withRestarts(
        warning(condition),
        muffle = function(handler, cond) handler(cond)
    )
}

#' @export
raise.error <- function(condition){
    withRestarts(
        stop(condition),
        muffle = function(handler, cond) handler(cond)
    )
}