
skip_if_not_installed("mocktest", "0.1.0")

Sys.setenv(TESTTHAT_PKG = "mocktest")

test_that("can change value of internal function", {

  val <- mocktest:::test_non_exported()

  res <- mockthat::with_mock(
    test_non_exported_nested = function() 5,
    mocktest:::test_non_exported()
  )

  expect_equal(res, 5)
  expect_error(expect_equal(res, val), class = "expectation_failure")
  expect_equal(mocktest:::test_non_exported(), val)

  # and value is restored on error
  expect_error(
    mockthat::with_mock(
      test_non_exported_nested = function() 5,
      stop("!")
    )
  )

  expect_equal(mocktest:::test_non_exported(), val)
})

test_that("mocks can access local variables", {

  x <- 5

  mockthat::with_mock(
    test_non_exported_nested = function() x,
    expect_equal(mocktest:::test_non_exported(), 5)
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

  expect_error(
    mockthat::with_mock(
      head = function(...) tail(...), test_utils(mtcars)
    ),
    "Can't mock functions in utils package"
  )

  expect_error(
    mockthat::with_mock(
      `utils::head` = function(...) utils::tail(...), test_utils(mtcars)
    ),
    "Can't mock functions in utils package"
  )

  expect_error(
    mockthat::with_mock(
      tail = function(...) head(...), test_utils(mtcars)
    ),
    "Can't mock functions in utils package"
  )

  expect_error(
    mockthat::with_mock(
      `utils::tail` = function(...) utils::head(...), test_utils(mtcars)
    ),
    "Can't mock functions in utils package"
  )
})

test_that("can't mock non-existing", {

  expect_error(
    mockthat::with_mock(..bogus.. = identity, TRUE),
    "Function `\\.\\.bogus\\.\\.` not found in environment mocktest"
  )
})

test_that("can't mock non-function", {

  expect_error(
    mockthat::with_mock(test_symbol = FALSE, TRUE),
    "Function `test_symbol` not found in environment mocktest"
  )
})

test_that("empty or no-op mock", {

  warn <- paste("Not mocking anything. Please use named parameters to specify",
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
