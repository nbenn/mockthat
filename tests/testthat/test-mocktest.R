
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
