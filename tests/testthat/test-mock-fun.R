
url <- "https://eu.httpbin.org/get?foo=123"

test_that("create and query mock_fun", {

  mk <- mock("mocked request")
  dl <- function(url) curl::curl(url)

  expect_identical(
    with_mock(`curl::curl` = mk, dl(url)),
    "mocked request"
  )

  expect_type(mock_args(mk), "list")
  expect_length(mock_args(mk), 3L)

  expect_identical(mock_arg(mk, "url"), url)
  expect_identical(mock_arg(mk, "open"), "")

  skip_if(getRversion() < "3.6.0")
  expect_identical(mock_call(mk), str2lang("curl::curl(url = url)"))
})

test_that("mock_arg errs if mocked function wasn't called", {
  url <- "https://eu.httpbin.org/get?foo=123"
  mk <- mock("mocked request")
  with_mock(`curl::curl` = mk, {})
  expect_error(
    mock_arg(mk, "url"),
    "mocked function was never called"
  )
})
