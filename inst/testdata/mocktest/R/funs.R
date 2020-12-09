
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

show_struct <- utils::str

#' @importFrom utils head
test_utils <- function(x, how = c("struct", "head", "tail")) {

  switch(
    match.arg(how),
    struct = show_struct(x),
    head = head(x),
    tail = utils::tail(x)
  )
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
test_depends <- function(url, how = c("mem", "disk", "stream")) {

  switch(
    match.arg(how),
    mem = curl_dl_mem(url),
    disk = curl_fetch_disk(url),
    stream = curl::curl_fetch_stream(url)
  )
}

jsonlite_pretty <- jsonlite::prettify

#' @importFrom jsonlite minify
test_imports <- function(x, what = c("pretty", "mini", "conv")) {

  switch(
    match.arg(what),
    pretty = jsonlite_pretty(x),
    mini = minify(x),
    conv = jsonlite::fromJSON(x)
  )
}
