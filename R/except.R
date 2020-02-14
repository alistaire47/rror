#' Catch conditions
#'
#' @param expression Expression to evaluate within which to catch conditions
#' @param ... Named arguments where names are condition classes to catch, and
#'     arguments are functions to evaluate when that condition is raised.
#'     Functions should take a single parameter, which will be passed the
#'     condition object raised.
#'
#' @export
except <- function(expression, ...){
    dots <- lapply(list(...), function(f){
        function(cond){
            if (inherits(cond, "error")) {
                restart <- "exit"
            } else {
                restart <- "muffle"
            }
            invokeRestart(restart, handler = f, cond = cond)
        }
    })

    expr <- substitute(withRestarts(
        expr = expression,
        exit = function(handler, cond) handler(cond)
    ))

    do.call(withCallingHandlers, c(list(expr), dots))
}
