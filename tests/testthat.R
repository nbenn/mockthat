
# for running tests interactively, run
# load_all(rprojroot::is_r_package$find_file("inst", "testdata", "mocktest"))

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
