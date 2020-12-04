
skip_if_not_installed("withr")

url <- "https://eu.httpbin.org/get?foo=123"

is_ns_env <- function(x) isNamespace(environment(x))

test_that("local mock works", {

  expect_true(is_ns_env(curl::curl))

  dl <- function(url) curl::curl(url)
  mk <- local_mock(`curl::curl` = "mocked request")

  expect_identical(dl(url), "mocked request")
  expect_identical(mock_args(mk, "url"), url)
  expect_false(is_ns_env(curl::curl))
})

test_that("local mock is restored", {
  expect_true(is_ns_env(curl::curl))
})
