## Test environments
* local R installation, macOS 10.15, R 4.0.3
* GitHub Actions (ubuntu-16.04): 3.3, 3.4, 3.5, oldrel, release, devel
* GitHub Actions (windows): 3.6, release
* Github Actions (macOS): release
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

* Function mocking, intended to be run within the context of unit testing
  using `testthat` is powered by `utils::assignInNamespace()`. While the
  corresponding documentation cautions against use in production code, I would
  like to argue that

  1. Code executed during unit testing is not 'production code'
  1. Using `utils::assignInNamespace()` is less of an API violation than the
     code currently powering [`testthat::with_mock()`](https://github.com/r-lib/testthat/blob/9af829bf3ee909d7ae8870a5123f24d1dd5c6c2a/src/reassign.c#L1-L24).
  1. The current CRAN-available alternatives `mockery` and `mockr` both suffer
     from the limitation that they cannot stub out functions within the
     context of S3 dispatch
