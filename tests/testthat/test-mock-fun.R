
url <- "https://eu.httpbin.org/get?foo=123"

test_that("create and query mock_fun", {

  mk <- mock("mocked request")
  dl <- function(url) curl::curl(url)

  expect_identical(
    with_mock(`curl::curl` = mk, dl(url)),
    "mocked request"
  )

  expect_identical(mock_args(mk, "url"), url)
  expect_identical(mock_args(mk, "open"), "")

  skip_if(getRversion() < "3.6.0")
  expect_identical(mock_call(mk), str2lang("curl::curl(url = url)"))
})
