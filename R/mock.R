
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
#'
#' url <- "https://eu.httpbin.org/get?foo=123"
#' mok <- function(...) "mocked request"
#'
#' with_mock(
#'   `curl::curl_fetch_memory` = mok,
#'   curl::curl_fetch_memory(url)
#' )
#'
#' dl_fun <- function(x) curl::curl_fetch_memory(x)
#'
#' with_mock(
#'   `curl::curl_fetch_memory` = mok,
#'   dl_fun(url)
#' )
#'
#' json <- function(...) '["mocked request"]'
#'
#' with_mock(
#'   `curl::curl` = json,
#'   jsonlite::fromJSON(url)
#' )
#'
#' with_mock(
#'   `curl::curl` = json,
#'   parse_and_simplify = function(txt, ...) gsub('\\[?\\"\\]?', "", txt),
#'   jsonlite::fromJSON(url),
#'   mock_env = "jsonlite"
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

  on.exit(lapply(mocks, reset_mock))
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

extract_mocks <- function(funs, env) {

  if (is.environment(env)) {
    env <- environmentName(env)
  }

  Map(
    extract_mock,
    names(funs),
    funs,
    MoreArgs = list(env = env)
  )
}

extract_mock <- function(fun_name, new_val, env) {

  rgx <- "^(?:(.*[^:])::(?:[:]?))?(.*)$"

  pkg_name <- gsub(rgx, "\\1", fun_name)
  fun_name <- gsub(rgx, "\\2", fun_name)

  if (pkg_name == "") {
    pkg_name <- env
  }

  env <- asNamespace(pkg_name)
  fun <- get0(fun_name, envir = env, mode = "function")

  if (is.null(fun)) {

    stop(
      "Function `", fun_name, "` not found in environment ",
      environmentName(env), ".",
      call. = FALSE
    )
  }

  mock(fun_name, fun, new_val)
}

mock <- function(name, fun, new) {

  env <- environment(fun)

  stopifnot(isNamespace(env), is.function(fun), is.function(new))

  nsn <- getNamespaceName(env)

  if (is_base_pkg(nsn)) {

    stop(
      "Can't mock functions in ", nsn, " package.",
      call. = FALSE
    )
  }

  structure(
    list(
      name = name,
      env = env,
      orig_value = fun,
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
