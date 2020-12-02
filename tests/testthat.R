
# pkg_dir <- rprojroot::is_r_package$find_file("inst", "testdata", "mocktest")
# for rebuilding, run
# devtools::document(pkg_dir)
# devtools::check(pkg_dir)
# devtools::build(pkg_dir)
# for running tests interactively, run
# Sys.setenv(TESTTHAT_PKG = "mockthat")
# library(testthat)
# load_all(pkg_dir)

# according to https://stackoverflow.com/a/27994299/3855417
Sys.setenv("R_TESTS" = "")

test_pkg <- "mocktest"

files <- list.files(
  system.file("testdata", package = "mockthat"),
  pattern = paste(test_pkg, "[0-9]+\\.[0-9]+\\.[0-9]+\\.tar\\.gz", sep = "_"),
  full.names = TRUE
)

install.packages(files[length(files)], repos = NULL, type = "source")

library(testthat)
library(test_pkg, character.only = TRUE)

test_check(test_pkg)
