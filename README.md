
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mockthat

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/nbenn/mockthat/workflows/build/badge.svg)](https://github.com/nbenn/mockthat/actions?query=workflow%3Abuild)
[![R check
status](https://github.com/nbenn/mockthat/workflows/check/badge.svg)](https://github.com/nbenn/mockthat/actions?query=workflow%3Acheck)
[![pkgdown build
status](https://github.com/nbenn/mockthat/workflows/pkgdown/badge.svg)](https://github.com/nbenn/mockthat/actions?query=workflow%3Apkgdown)
[![covr
status](https://github.com/nbenn/mockthat/workflows/coverage/badge.svg)](https://github.com/nbenn/mockthat/actions?query=workflow%3Acoverage)
[![Codecov test
coverage](https://codecov.io/gh/nbenn/mockthat/branch/master/graph/badge.svg?token=9v2gSCz5K5)](https://codecov.io/gh/nbenn/mockthat)
<!-- badges: end -->

With version 3.0.0 of `testthat`, mocking capabilities provided by
[`testthat::with_mock()`](https://testthat.r-lib.org/reference/with_mock.html)
and
[`testthat::local_mock()`](https://testthat.r-lib.org/reference/with_mock.html)
have been deprecated under edition 3. This leaves implementation of
function mocking for unit testing to third-party packages, of which two
have been published on CRAN:
[`mockery`](https://cran.r-project.org/package=mockery) and
[`mockr`](https://cran.r-project.org/package=mockr). While all currently
available mocking implementations have their limitations, what sets
`mockthat` apart from `mockery` and `mockr` is coping with S3 dispatch
(see example below).

## Installation

You can install the development version of `mockthat` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nbenn/mockthat")
```

A release version will be submitted to
[CRAN](https://CRAN.R-project.org) shortly.

## Example

Mocking in the context of unit testing refers to temporarily replacing a
piece of functionality (that might be part of the package being tested
or potentially even part of a downstream dependency) in order to cope
with limited infrastructure in testing environments (for example absence
of a live Internet connection).

For a function `download_data()` implemented as

``` r
download_data <- function(url) {
  curl::curl_fetch_memory(url)
}
```

we do not want to have to rely on a live internet connection for writing
a unit test. With help of `mockthat`, we can substitute
[`curl::curl_fetch_memory()`](https://rdrr.io/cran/curl/man/curl_fetch.html)
with a stub that simply returns a constant.

``` r
library(mockthat)

url <- "https://eu.httpbin.org/get?foo=123"

with_mock(
  `curl::curl_fetch_memory` = function(...) '["mocked request"]',
  download_data(url)
)
#> [1] "[\"mocked request\"]"
```

As mentioned above, the main point of differentiation of `mockthat` over
the other available packages `mockery` and `mockr` is stubbing out
functions in the context of S3 dispatch. Assuming the following set-up,

``` r
gen <- function(x) UseMethod("gen")

met <- function(x) foo(x)

foo <- function(x) stop("foo")

.S3method("gen", "cls", met)

x <- structure(123, class = "cls")

gen(x)
#> Error in foo(x): foo
```

`mockthat::with_mock()` can be used to catch the call to `foo()` and
therefore prevent the error from being thrown.

``` r
mockthat::with_mock(
  foo = function(x) "bar",
  met(x)
)
#> [1] "bar"
```

This is not possible with the current implementation of
[`mockr::with_mock()`](https://krlmlr.github.io/mockr/reference/with_mock.html).

``` r
mockr::with_mock(
  foo = function(x) "bar",
  met(x)
)
#> Warning: Replacing functions in evaluation environment: `foo()`
#> Warning: The code passed to `with_mock()` must be a braced expression to get
#> accurate file-line information for failures.
#> [1] "bar"
```

And with the current API of
[`mockery::stub()`](https://rdrr.io/cran/mockery/man/stub.html) it is
unclear how the `depth` argument should be chosen, as the function
`gen()` does not contain a call to `met()`. Trying a range of sensible
values does not yield the desired result.

``` r
for (depth in seq_len(3L)) {
  mockery::stub(gen, "foo", "bar", depth = depth)
  tryCatch(met(x), error = function(e) message("depth ", depth, ": nope"))
}
#> depth 1: nope
#> depth 2: nope
#> depth 3: nope
```

Borrowing from `mockery`, `mockthat` also allows for creating mock
objects (with class attribute `mock_fun`), which allow capture of the
call for later examination.

``` r
mk <- mock("mocked request")
dl <- function(url) curl::curl(url)

with_mock(`curl::curl` = mk, dl(url))
#> [1] "mocked request"

mock_call(mk)
#> curl::curl(url = url)
mock_args(mk)
#> $url
#> [1] "https://eu.httpbin.org/get?foo=123"
#> 
#> $open
#> [1] ""
#> 
#> $handle
#> <curl handle> (empty)
```

In addition to `with_mock()`, `mockthat` also offers a `local_mock()`
function, again, mimicking the deprecated `testthat` function, which
keeps the mocks in place for the life-time of the environment passed as
`local_env` argument (or if called from the global environment, until
`withr::deferred_run()` is executed). Mock objects as shown above are
created (and returned invisibly) for all non-function objects passed as
`...`.

``` r
tmp <- new.env()
mk <- local_mock(`curl::curl` = "mocked request", local_env = tmp)
dl(url)
#> [1] "mocked request"
mock_arg(mk, "url")
#> [1] "https://eu.httpbin.org/get?foo=123"
```
