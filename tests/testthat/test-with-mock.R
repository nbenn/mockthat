
url <- "https://eu.httpbin.org/get?foo=123"

test_that("can mock top-level", {

  mok <- function(...) "mocked request"

  res <- with_mock(
    `curl::curl_fetch_memory` = mok,
    curl::curl_fetch_memory(url)
  )

  expect_identical(res, "mocked request")
})

test_that("can access calling env", {

  asw <- "mocked request"
  mok <- function(...) asw

  res <- with_mock(
    `curl::curl_fetch_memory` = mok,
    curl::curl_fetch_memory(url)
  )

  expect_identical(res, asw)
})

test_that("can mock nested", {

  mok <- function(...) "mocked request"
  dlf <- function(x) curl::curl_fetch_memory(x)

  res <- with_mock(
    `curl::curl_fetch_memory` = mok,
    dlf(url)
  )

  expect_identical(res, "mocked request")
})

test_that("visibility propagates", {

  expect_invisible(
    res <- with_mock(
      `curl::curl_fetch_memory` = quote(invisible(NULL)),
      curl::curl_fetch_memory(url)
    )
  )

  expect_null(res)
})
