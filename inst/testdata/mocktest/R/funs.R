
test_non_exported <- function() {
  test_non_exported_nested()
}

test_non_exported_nested <- function() 10

#' Exported test function
#'
#' @description
#' This function only serves to test mocking within an exported function.
#'
#' @return 10 (as scalar numeric)
#'
#' @export
test_exported <- function() {
  test_non_exported()
}
