#' Create a condition object
#'
#' Condition objects are classed objects which can be raised when desired.
#' `condition` objects are the most basic form; message, warning, and error
#' conditions are more specific. Additional classes can be added to make
#' condition handling easier and more targeted.
#'
#' @param message A message to display when the condition is raised
#' @param class Character vector of classes to append to the condition class
#' @param capture_call Logical. When raised, should the condition include the call?
#' @param ... Additional arguments to append to the condition object
#'
#' @export
condition <- function(message, class = NULL, capture_call = TRUE, ...){
    structure(
        list(
            message = message,
            ...
        ),
        capture_call = capture_call,
        class = c(class, "condition")
    )
}

#' @rdname condition
#' @export
message_condition = function(message, class = NULL, capture_call = TRUE, ...){
    condition(
        message = message,
        class = c(class, "message"),
        capture_call = capture_call,
        ...
    )
}

#' @rdname condition
#' @export
warning_condition = function(message, class = NULL, capture_call = TRUE, ...){
    condition(
        message = message,
        class = c(class, "warning"),
        capture_call = capture_call,
        ...
    )
}

#' @rdname condition
#' @export
error_condition = function(message, class = NULL, capture_call = TRUE, ...){
    condition(
        message = message,
        class = c(class, "error"),
        capture_call = capture_call,
        ...
    )
}