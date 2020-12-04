
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
#' deprecated from testthat, leaving it to third party packages to replace this
#' feature. This mocking implementation is powered by
#' [utils::assignInNamespace()] and therefore, caveats outlined in the
#' corresponding documentation apply here too.
#'
#' @param ... Named parameters redefine mocked functions, unnamed parameters
#' will be evaluated after mocking the functions.
#' @param mock_env The environment in which to patch the functions,
#' defaults to the top-level environment. A string is interpreted as package
#' name.
#' @param eval_env Environment in which expressions passed as `...` are
#' evaluated, defaults to [parent.frame()].
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
#' mk <- mock("mocked request")
#' dl <- function(url) curl::curl(url)
#'
#' with_mock(`curl::curl` = mk, dl(url))
#'
#' mock_call(mk)
#' mock_args(mk)
#'
#' mk <- local_mock(`curl::curl` = "mocked request")
#' dl(url)
#'
#' mock_args(mk, "url")
#'
#' @return The result of the last unnamed argument passed as `...` (evaluated
#' in the environment passed as `eval_env`) in the case of `local_mock()` and
#' a list of functions or `mock_fun` objects (invisibly) in the case of
#' `local_mock()`.
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

  if (length(code) == 0) {

    warning(
      "Nothing to do with mocked functions. Please pass at least a single ",
      "unnamed argument as `...`.",
      call. = FALSE
    )
  }

  mfuns <- lapply(dots[!code_pos], eval, eval_env)
  mfuns <- lapply(mfuns, mock_expr, eval_env)
  mocks <- extract_mocks(mfuns, env = mock_env)

  on.exit(lapply(mocks, reset_mock))
  lapply(mocks, set_mock)

  for (expression in code[-length(code)]) {
    eval(expression, eval_env)
  }

  # Isolate last item for visibility
  eval(code[[length(code)]], eval_env)
}

#' A `local_*()` variant of `with_mock()` is available as `local_mock()`, in
#' line with current `testthat` practice. Powered by [withr::defer()], mocks
#' are active for the life-time of the environment passed as `local_env`. If
#' non-function objects are passed as `...`, `mock_fun` objects are created (
#' and returned invisibly), which can be queried for call and argument
#' information after having been called.
#'
#' @param local_env Passed to [withr::defer()] as `envir` argument
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

#' @param expr Expression to be used as body of the function to be mocked.
#' @param env Environment used as ancestor to the mock function environment.
#'
#' @rdname mock
#' @export
mock <- function(expr, env = parent.frame()) {
  mock_quo(substitute(expr), env = env)
}

#' @param x Object of class `mock_fun` to be queried for call and argument
#' information.
#'
#' @rdname mock
#' @export
mock_call <- function(x) {

  if (is.list(x) && length(x) == 1L) {
    x <- x[[1L]]
  }

  stopifnot(is_mock_fun(x))

  get("call", envir = attr(x, "env"))
}

#' @param arg String-valued argument name to be retrieved.
#'
#' @rdname mock
#' @export
mock_args <- function(x, arg = NULL) {

  if (is.list(x) && length(x) == 1L) {
    x <- x[[1L]]
  }

  stopifnot(is_mock_fun(x))

  env <- attr(x, "env")
  fun <- get("fun", envir = env)

  called_args <- get("args", envir = env)
  formal_args <- formals(fun)

  if (is.character(arg)) {
    stopifnot(length(arg) == 1L)
    if (arg %in% names(called_args)) {
      return(called_args[[arg]])
    }
  }

  defaults <- setdiff(names(formal_args), names(called_args))

  formal_args[defaults] <- lapply(formal_args[defaults], eval,
                                  environment(fun))
  formal_args[names(called_args)] <- called_args

  if (is.character(arg)) {
    stopifnot(arg %in% names(formal_args))
    return(formal_args[[arg]])
  }

  formal_args
}

is_mock_fun <- function(x) inherits(x, "mock_fun")

mock_expr <- function(expr, env) {

  if (is.function(expr) || is_mock_fun(expr)) {
    return(expr)
  }

  mock_quo(expr, env)
}

mock_quo <- function(quo, env) {

  capt <- quote({
    call <<- match.call()
    args <<- lapply(as.list(call)[-1L], eval, parent.frame())
  })

  if (is.function(quo)) {
    par <- environment(quo)
    quo <- body(quo)
  } else {
    par <- env
  }

  env <- list2env(list(call = NULL, args = NULL), parent = par)

  if (is.language(quo) && identical(quo[[1L]], quote(`{`))) {
    quo <- quo[-1L]
  } else {
    quo <- as.expression(quo)
  }

  capt[seq_along(quo) + length(capt)] <- quo

  structure(capt, env = env, class = "mock_fun")
}

pkg_env <- function() {

  if (requireNamespace("testthat", quietly = TRUE)) {
    res <- testthat::testing_package()
  } else {
    res <- Sys.getenv("TESTTHAT_PKG")
  }

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

  new_mock(fun_name, fun, new_val)
}

new_mock <- function(name, fun, new) {

  env <- environment(fun)

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
  x %in% rownames(utils::installed.packages(priority = "base"))
}
