
test_non_exported <- function() {
  test_non_exported_nested()
}

test_non_exported_nested <- function() 10L

test_symbol <- 0L

#' Exported test functions
#'
#' @description
#' These functions only serves to test mocking capabilities of `mockthat`. The
#' exported function `test_exported()` contains a call to the non-exported
#' function `test_non_exported()`, `test_base()` contains calls to base
#' functions [base::isTRUE()] and [base::all.equal()] and `test_s3()` can be
#' used to test mocking in the context of S3 dispatch.
#'
#' @return
#' * `test_exported()`: 10 (as scalar integer)
#' * `test_base()`: Logical flag indicating whether `base::all.equal()`
#'   returned `TRUE`.
#' * `test_s3()`: 10 (as scalar integer) if dispatched on an object of class
#'   `test_cls` and 5 (as scalar integer) if dispatched on any other class
#'
#' @rdname test_funs
#' @export
test_exported <- function() {
  test_non_exported()
}

#' @param x,y Forwarded to [base::all.equal()]
#' @rdname test_funs
#' @export
test_base <- function(x, y) isTRUE(base::all.equal(x, y))

#' @importFrom utils head
test_utils <- function(x, n = 2L) {
  rbind(head(x, n = n), utils::tail(x, n = n))
}

#' @rdname test_funs
#' @export
test_s3 <- function(x) UseMethod("test_s3", x)

#' @export
test_s3.test_cls <- function(x) test_non_exported()

#' @export
test_s3.default <- function(x) 5L

curl_dl_mem <- curl::curl_fetch_memory

#' @importFrom curl curl_fetch_disk
test_depends <- function(url, how = c("mem", "disk")) {

  switch(
    how,
    mem = curl_dl_mem(url),
    disk = curl_fetch_disk(url)
  )
}

jsonlite_flatten <- jsonlite::flatten

#' @importFrom jsonlite unbox
test_imports <- function(x, what = c("flat", "ubox")) {

  switch(
    what,
    flat = jsonlite_flatten(url),
    ubox = unbox(url)
  )
}
