
test_that("can mock top-level", {

  url <- "https://eu.httpbin.org/get?foo=123"
  mok <- function(...) "mocked request"

  res <- with_mock(
    `curl::curl_fetch_memory` = mok,
    curl::curl_fetch_memory(url)
  )

  expect_identical(res, "mocked request")
})
