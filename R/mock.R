
#' Mock functions in a package.
#'
#' @description
#' Mocking allows you to temporary replace the implementation of functions
#' within a package, which useful for testing code that relies on functions
#' that are slow, have unintended side effects or access resources that may
#' not be available when testing.
#'
#' Up until recently, such capability was offered via [testthat::with_mock()],
#' but with release of version 3.0.0 and introduction of edition 3, this was
#' deprecated from 'testthat', leaving it to third party packages to replace
#' this feature. Powered by [utils::assignInNamespace()], this mocking
#' implementation can be used to stub out both exported and non-exported
#' functions from a package, as well as functions explicitly imported from
#' other packages using either `importFrom` directives or namespaced function
#' calls using `::`.
#'
#' @details
#' Borrowing the API from the now-deprecated [testthat::with_mock()], named
#' arguments passed as `...` are used to define functions to be mocked, where
#' names specify the target functions and the arguments themselves are used as
#' replacement functions. Unnamed arguments passed as `...` will be evaluated
#' in the environment specified as `eval_env` using the mocked functions.
#' Functions to be stubbed should be specified as they would be used in package
#' core. This means that when a function from a third party package is
#' imported, prefixing the function name with `pkg_name::` will not give the
#' desired result. Conversely, if the function is not imported, the package
#' prefix is of course required. On exit of `with_mock()`, the mocked functions
#' are reverted to their original state.
#'
#' Replacement functions can either be specified as complete functions, or as
#' either quoted expressions, subsequently used as function body or objects
#' used as return values. If functions are created from return values or
#' complete function bodies, they inherit the signatures from the respective
#' functions they are used to mock, alongside the ability to keep track of
#' how they are subsequently called. A constructor for such mock-objects is
#' available as `mock()`, which quotes the expression passed as `expr`.
#'
#' If mocking is desirable for multiple separate calls to the function being
#' tested, `local_mock()` is available, which holds onto the mocked state for
#' the lifetime of the environment passed as `local_env` using
#' [withr::defer()]. Unlike `with_mock()`, which returns the result of
#' evaluating the last unnamed argument passed as `...`, `local_mock()`
#' (invisibly) returns the functions used for mocking, which if not fully
#' specified as functions, will be mock-objects described in the previous
#' paragraph.
#'
#' @param ... Named parameters redefine mocked functions, unnamed parameters
#' will be evaluated after mocking the functions.
#' @param mock_env The environment in which to patch the functions,
#' defaults to either the package namespace when the environment variable
#' `TESTTHAT_PKG` is set pr the calling environment. A string is interpreted
#' as package
#' name.
#' @param eval_env Environment in which expressions passed as `...` are
#' evaluated, defaults to [base::parent.frame()].
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
#'   `curl::curl` = '["mocked request"]',
#'   jsonlite::fromJSON(url)
#' )
#'
#' with_mock(
#'   `curl::curl` = quote({
#'     x <- "mocked request"
#'     paste0('["', x, '"]')
#'   }),
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
#' dl <- function(x) curl::curl(x)
#'
#' local({
#'   mk <- local_mock(`curl::curl` = "mocked request")
#'   list(dl(url), mock_arg(mk, "url"))
#' })
#'
#' @return
#' The result of the last unnamed argument passed as `...` (evaluated in the
#' environment passed as `eval_env`) in the case of `local_mock()` and a list
#' of functions or `mock_fun` objects (invisibly) for calls to `local_mock()`.
#'
#' @rdname mock
#' @export
with_mock <- function(..., mock_env = pkg_env(), eval_env = parent.frame()) {

  dots <- eval(substitute(alist(...)))
  mock_qual_names <- names(dots)

  if (all(mock_qual_names == "")) {

    warning(
      "Not mocking anything. Please use named arguments to specify the ",
      "functions you want to mock.",
      call. = FALSE
    )

    code_pos <- rep(TRUE, length(dots))

  } else {

    code_pos <- (mock_qual_names == "")
  }

  code <- dots[code_pos]

  mfuns <- lapply(dots[!code_pos], eval, eval_env)
  mfuns <- lapply(mfuns, mock_expr, eval_env)
  mocks <- extract_mocks(mfuns, env = mock_env)

  on.exit(lapply(mocks, reset_mock))
  lapply(mocks, set_mock)

  if (length(code) > 0) {

    for (expression in code[-length(code)]) {
      eval(expression, eval_env)
    }

    # Isolate last item for visibility
    eval(code[[length(code)]], eval_env)
  }
}

#' A `local_*()` variant of `with_mock()` is available as `local_mock()`, in
#' line with current `testthat` practice. Powered by [withr::defer()], mocks
#' are active for the life-time of the environment passed as `local_env`. If
#' non-function objects are passed as `...`, `mock_fun` objects are created (
#' and returned invisibly by [mock()]), which can be queried for call and
#' argument information after having been called.
#'
#' @param local_env Passed to [withr::defer()] as `envir` argument (defaults
#' to the values passed as `eval_env`)
#'
#' @rdname mock
#' @export
local_mock <- function(..., mock_env = pkg_env(), eval_env = parent.frame(),
                       local_env = eval_env) {

  if (!requireNamespace("withr", quietly = TRUE)) {

    stop(
      "Local mocking requires the \"withr\" package.",
      call. = FALSE
    )
  }

  mfuns <- lapply(eval(substitute(alist(...))), eval, eval_env)
  mfuns <- lapply(mfuns, mock_expr, eval_env)
  mocks <- extract_mocks(mfuns, env = mock_env)

  withr::defer(lapply(mocks, reset_mock), local_env)
  lapply(mocks, set_mock)

  invisible(mfuns)
}

pkg_env <- function() {

  if (requireNamespace("testthat", quietly = TRUE)) {
    res <- testthat::testing_package()
  } else {
    res <- Sys.getenv("TESTTHAT_PKG")
  }

  if (identical(res, "")) {
    parent.frame(2L)
  } else {
    asNamespace(res)
  }
}

extract_mocks <- function(funs, env) {
  Map(extract_mock, names(funs), funs, MoreArgs = list(env = env))
}

extract_mock <- function(fun_name, new_val, env) {

  rgx <- "^(?:(.*[^:])::(?:[:]?))?(.*)$"

  pkg_name <- gsub(rgx, "\\1", fun_name)
  fun_name <- gsub(rgx, "\\2", fun_name)

  if (identical(pkg_name, "")) {

    if (is.null(null_or_ns(env))) {
      env <- environment(get0(fun_name, envir = env, mode = "function"))
    } else {
      env <- null_or_ns(env)
    }

  } else {

    env <- asNamespace(pkg_name)
  }

  fun <- get0(fun_name, envir = env, mode = "function")

  if (is.null(fun)) {

    stop(
      "Function `", fun_name, "` not found in environment ",
      environmentName(env), ".",
      call. = FALSE
    )
  }

  fun_env <- environment(fun)
  imp_env <- imports_env(env)

  if (fun_exists(fun_name, env) && isNamespace(fun_env) &&
      !fun_exists(fun_name, fun_env)) {

    new_mock(fun_name, fun, new_val, env)

  } else if (is.environment(imp_env) && fun_exists(fun_name, imp_env)) {

    new_mock(fun_name, fun, new_val, imp_env)

  } else {

    new_mock(fun_name, fun, new_val, environment(fun))
  }
}

fun_exists <- function(name, envir, inherits = FALSE) {
  exists(name, envir = envir, mode = "function", inherits = inherits)
}

null_or_ns <- function(x) {

  if (is.character(x) || is.name(x)) {
    x <- getNamespace(x)
  }

  if (isNamespace(x)) {
    return(x)
  }

  NULL
}

imports_env <- function(x) {

  if (!isNamespace(x)) {
    return(NULL)
  }

  res <- parent.env(x)
  nme <- paste("imports", environmentName(x), sep = ":")

  if (identical(attr(res, "name"), nme)) {
    return(res)
  }

  NULL
}

new_mock <- function(name, fun, new, env) {

  if (isNamespace(env) && is_base_pkg(getNamespaceName(env))) {

    stop(
      "Can't mock functions in ", getNamespaceName(env), " package.",
      call. = FALSE
    )
  }

  if (inherits(new, "mock_fun")) {

    mok_env <- attr(new, "env")
    assign("fun", fun, envir = mok_env)

    new <- new_function(formals(fun), new, mok_env)
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

new_function <- function(formals = NULL, body = NULL, env = parent.frame()) {

  res <- function(){}

  if (!is.null(formals)) {
    formals(res) <- formals
  }

  if (!is.null(body)) {
    body(res) <- body
  }

  environment(res) <- env

  res
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
  x %in% c("base", "tools", "utils", "grDevices", "graphics",
           "stats", "datasets", "methods", "grid", "splines", "stats4",
           "tcltk", "compiler", "parallel")
}
