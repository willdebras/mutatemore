#' mutate_cond
#'
#' A function that mutates data frames but only acts on the rows satisfying the condition
#'
#' @param .data a dataframe or data table
#' @param condition an expreessed condition to mutate on
#' @param ... additional conditions separated by commas
#'
#' @return a mutated dataframe
#' @export
#' @importFrom dplyr mutate
#'
#' @examples DF %>% mutate_cond(measure == 'exit', qty.exit = qty, cf = 0, delta.watts = 13)
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
