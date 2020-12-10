
pkg_dir <- system.file("testdata", "mocktest", package = "mockthat")
pkg_nme <- pkgload::pkg_name(pkg_dir)

pkgload::load_all(pkg_dir, attach_testthat = FALSE)

withr::defer(pkgload::unload(pkg_nme))
withr::local_envvar(TESTTHAT_PKG = pkg_nme)

test_that("can change value of internal function", {

  val <- test_non_exported()

  res <- mockthat::with_mock(
    test_non_exported_nested = function() 5,
    test_non_exported()
  )

  expect_equal(res, 5)
  expect_error(expect_equal(res, val), class = "expectation_failure")
  expect_equal(test_non_exported(), val)

  # and value is restored on error
  expect_error(
    mockthat::with_mock(
      test_non_exported_nested = function() 5,
      stop("!")
    )
  )

  expect_equal(test_non_exported(), val)
})

test_that("mocks can access local variables", {

  x <- 5

  mockthat::with_mock(
    test_non_exported_nested = function() x,
    expect_equal(test_non_exported(), 5)
  )
})

test_that("non-empty mock with return value", {

  expect_true(
    mockthat::with_mock(test_non_exported = function(...) FALSE, TRUE)
  )
})

test_that("can't mock base functions", {

  expect_error(
    mockthat::with_mock(
      all.equal = function(x, y, ...) TRUE, test_base(3, "a")
    ),
    "Can't mock functions in base package"
  )

  expect_error(
    mockthat::with_mock(
      `base::all.equal` = function(x, y, ...) TRUE, test_base(3, "a")
    ),
    "Can't mock functions in base package"
  )

  expect_error(
    mockthat::with_mock(
      isTRUE = function(x, y, ...) FALSE, test_base(3, "a")
    ),
    "Can't mock functions in base package"
  )

  expect_error(
    mockthat::with_mock(
      `base::isTRUE` = function(x, y, ...) FALSE, test_base(3, "a")
    ),
    "Can't mock functions in base package"
  )
})

test_that("can sometimes mock utils functions", {

  res <- mockthat::with_mock(
    show_struct = "bar",
    test_utils("foo", "struct")
  )

  expect_identical(res, "bar")

  expect_error(
    mockthat::with_mock(str = "bar", test_utils("foo", "struct")),
    "Can't mock functions in utils package"
  )

  expect_error(
    mockthat::with_mock(`utils::str` = "bar", test_utils("foo", "struct")),
    "Can't mock functions in utils package"
  )

  res <- mockthat::with_mock(
    head = "bar",
    test_utils("foo", "head")
  )

  expect_identical(res, "bar")

  expect_error(
    mockthat::with_mock(`utils::head` = "bar", test_utils("foo", "head")),
    "Can't mock functions in utils package"
  )

  expect_error(
    mockthat::with_mock(tail = "bar", test_utils("foo", "tail")),
    "Can't mock functions in utils package"
  )

  expect_error(
    mockthat::with_mock(`utils::tail` = "bar", test_utils("foo", "tail")),
    "Can't mock functions in utils package"
  )
})

test_that("can't mock non-existing", {

  expect_error(
    mockthat::with_mock(..bogus.. = identity, TRUE),
    paste("Function `\\.\\.bogus\\.\\.` not found in environment", pkg_nme)
  )
})

test_that("can't mock non-function", {

  expect_error(
    mockthat::with_mock(test_symbol = FALSE, TRUE),
    paste("Function `test_symbol` not found in environment", pkg_nme)
  )
})

test_that("empty or no-op mock", {

  warn <- paste("Not mocking anything. Please use named arguments to specify",
                "the functions you want to mock.")

  expect_warning(expect_null(mockthat::with_mock()), warn, fixed = TRUE)

  expect_warning(expect_true(mockthat::with_mock(TRUE)), warn, fixed = TRUE)
})

test_that("visibility", {

  expect_warning(
    expect_false(withVisible(mockthat::with_mock())$visible)
  )

  res <- withVisible(
    mockthat::with_mock(
      test_non_exported = function() {},
      TRUE
    )
  )

  expect_true(res$visible)

  res <- withVisible(
    mockthat::with_mock(
      test_non_exported = function() {},
      invisible(5)
    )
  )

  expect_false(res$visible)
})

test_that("multiple return values", {

  expect_true(
    mockthat::with_mock(FALSE, TRUE, test_non_exported = function() {})
  )

  expect_equal(
    mockthat::with_mock(3, test_non_exported = function() {}, 5),
    5
  )
})

test_that("can access variables defined in function", {

  x <- 5

  expect_equal(
    mockthat::with_mock(x, test_non_exported = function() {}),
    5
  )
})

test_that("can mock if package is not loaded", {

  if ("package:jsonlite" %in% search()) {
    skip("jsonlite is loaded")
  }

  skip_if_not_installed("jsonlite")

  mockthat::with_mock(
    `jsonlite::toJSON` = identity,
    expect_identical(jsonlite::toJSON, identity)
  )
})

test_that(
  "changes to variables are preserved between calls and visible outside", {

  x <- 1

  mockthat::with_mock(
    test_non_exported = function() {},
    x <- 3,
    expect_equal(x, 3)
  )

  expect_equal(x, 3)
})

test_that("mock within S3 dispatch", {

  x <- structure(1L, class = "test_cls")

  expect_identical(test_s3(x), 10L)
  expect_identical(mockthat::with_mock(test_non_exported = 5L, test_s3(x)), 5L)

  skip_if(getRversion() < "3.5.0")

  expect_identical(test_s3(1L), 5L)
  expect_identical(mockthat::with_mock(test_s3.default = 2L, test_s3(1L)), 2L)
})

test_that("mock depends", {

  res <- mockthat::with_mock(
    curl_dl_mem = "bar",
    test_depends("foo", "mem")
  )

  expect_identical(res, "bar")

  res <- mockthat::with_mock(
    curl_fetch_disk = "bar",
    test_depends("foo", "disk")
  )

  expect_identical(res, "bar")

  res <- mockthat::with_mock(
    `curl::curl_fetch_stream` = "bar",
    test_depends("foo", "stream")
  )

  expect_identical(res, "bar")
})

test_that("mock imports", {

  res <- mockthat::with_mock(
    jsonlite_pretty = "bar",
    test_imports("foo", "pretty")
  )

  expect_identical(res, "bar")

  res <- mockthat::with_mock(
    minify = "bar",
    test_imports("foo", "mini")
  )

  expect_identical(res, "bar")

  res <- mockthat::with_mock(
    `jsonlite::fromJSON` = "bar",
    test_imports("foo", "conv")
  )

  expect_identical(res, "bar")
})
