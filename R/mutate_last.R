#' mutate_last
#'
#'A function like mutate but operates only within a group_by and only operates on the last group rather than every group.
#'
#'
#' @param .data a dataframe or data table
#' @param ... conditions or set of conditions to mutate on last/greatest group
#'
#' @return a mutated dataframe or data table
#' @export
#' @importFrom dplyr n_groups
#'
#' @examples
#'
#' DF %>%
#' group_by(is.exit = measure == 'exit') %>%
#'   mutate_last(qty.exit = qty, cf = 0, delta.watts = 13) %>%
#'   ungroup() %>%
#'   select(-is.exit)
#'

mutate_last <- function(.data, ...) {
  n <- n_groups(.data)
  indices <- attr(.data, "indices")[[n]] + 1
  .data[indices, ] <- .data[indices, ] %>% mutate(...)
  .data
}
