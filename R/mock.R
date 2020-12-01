
#' Mock functions in a package.
#'
#' @description
#' Mocking allows you to temporary replace the implementation of functions
#' within a package, which useful for testing code that relies on functions
#' that are slow, have unintended side effects or access resources that may
#' not be available when testing.
#'
#' This works by using some C code to temporarily modify the mocked function
#' _in place_. On exit, all functions are restored to their previous state.
#' This is somewhat abusive of R's internals so use with care. In particular,
#' functions in base packages cannot be mocked; to work around you'll need to
#' make a wrapper function in your own package..
#'
#' @param ... Named parameters redefine mocked functions, unnamed parameters
#'   will be evaluated after mocking the functions
#' @param mock_env The environment in which to patch the functions,
#'   defaults to the top-level environment.  A character is interpreted as
#'   package name.
#' @param eval_env Environment in which expressions passed as `...` are
#'   evaluated, defaults to [parent.frame()].
#'
#' @examples
#' add_one <- function(x) x + 1
#' all.equal(add_one(2), 3)
#' with_mock(
#'   add_one = function(x) x - 1,
#'   all.equal(add_one(2), 1)
#' )
#' square_add_one <- function(x) add_one(x)^2
#' all.equal(square_add_one(2), 9)
#' all.equal(
#'   with_mock(
#'     add_one = function(x) x - 1,
#'     square_add_one(2)
#'   ),
#'   1
#' )
#'
#' @return The result of the last unnamed argument passed as `...` (evaluated
#'   in the environment passed as `eval_env`)
#'
#' @export
#'
with_mock <- function(..., mock_env = pkg_env(), eval_env = parent.frame()) {

  dots <- eval(substitute(alist(...)))
  mock_qual_names <- names(dots)

  if (all(mock_qual_names == "")) {

    warning(
      "Not mocking anything. Please use named parameters to specify the ",
      "functions you want to mock.",
      call. = FALSE
    )

    code_pos <- rep(TRUE, length(dots))

  } else {

    code_pos <- (mock_qual_names == "")
  }

  code <- dots[code_pos]

  mock_funs <- lapply(dots[!code_pos], eval, eval_env)
  mocks <- extract_mocks(mock_funs, env = mock_env)

  on.exit(lapply(mocks, reset_mock), add = TRUE)
  lapply(mocks, set_mock)

  # Evaluate the code
  if (length(code) > 0) {
    for (expression in code[-length(code)]) {
      eval(expression, eval_env)
    }
    # Isolate last item for visibility
    eval(code[[length(code)]], eval_env)
  }
}

pkg_env <- function() {

  res <- testthat::testing_package()

  if (identical(res, "")) {
    topenv()
  } else {
    asNamespace(res)
  }
}

pkg_rx <- ".*[^:]"
colons_rx <- "::(?:[:]?)"
name_rx <- ".*"
pkg_and_name_rx <- sprintf("^(?:(%s)%s)?(%s)$", pkg_rx, colons_rx, name_rx)

extract_mocks <- function(funs, env) {

  if (is.environment(env)) {
    env <- environmentName(env)
  }

  Map(
    extract_mock,
    stats::setNames(nm = names(funs)),
    funs,
    MoreArgs = list(env = env)
  )
}

extract_mock <- function(fun_name, new_val, env) {

  pkg_name <- gsub(pkg_and_name_rx, "\\1", fun_name)

  if (is_base_pkg(pkg_name)) {

    stop(
      "Can't mock functions in base packages (", pkg_name, ")",
      call. = FALSE
    )
  }

  name <- gsub(pkg_and_name_rx, "\\2", fun_name)

  if (pkg_name == "") {
    pkg_name <- env
  }

  env <- asNamespace(pkg_name)
  fun <- get0(name, envir = env, mode = "function")

  if (is.null(fun)) {

    stop(
      "Function ", name, " not found in environment ",
      environmentName(env), ".",
      call. = FALSE
    )

  } else if (is.primitive(fun)) {

    stop(
      "Can't mock functions of type ", typeof(fun),
      call. = FALSE
    )
  }

  mock(name = name, env = env, new = new_val)
}

mock <- function(name, env, new) {

  target_value <- get(name, envir = env, mode = "function")

  structure(
    list(
      name = name,
      env = environment(target_value),
      orig_value = target_value,
      new_value = new
    ),
    class = "mock"
  )
}

do_assign <- function(name, val, env) {

  if (isNamespace(env)) {

    utils::assignInNamespace(name, val, env)

  } else {

    assign(name, val, envir = env)
  }
}

set_mock <- function(mock) {
  do_assign(mock$name, mock$new_value, mock$env)
}

reset_mock <- function(mock) {
  do_assign(mock$name, mock$orig_value, mock$env)
}

is_base_pkg <- function(x) {
  x %in% rownames(utils::installed.packages(priority = "base"))
}

test_mock1 <- function() {
  test_mock2()
}

test_mock2 <- function() 10

some_symbol <- 42

mockee <- function() stop("Not mocking")

mockee2 <- function() stop("Not mocking (2)")

mockee3 <- function() mockee()

disp <- function(x) stats::sd(x)
