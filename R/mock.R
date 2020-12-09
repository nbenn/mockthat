
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
#' in the environment specified as `eval_env` using the mocked functions. On
#' exit of `with_mock()`, the mocked functions are reverted to their original
#' state.
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
#' Calls to mock-objects either constructed using `mock()` or returned by
#' `local_mock()` can keep track of how they were called and functions
#' `mock_call()`, `mock_args()` and `mock_n_called()` can be used to retrieve
#' related information:
#'
#' * `mock_call()`: retrieves the call captured by [base::match.call()]
#' * `mock_arg()`: retrieves the value of the argument with name passed as
#'   string-valued argument `arg`
#' * `mock_args()`: retrieves a list of all arguments used for calling the
#'   mocked function
#' * `mock_n_called()`: counts the number of times the mocked function was
#'   called
#'
#' Calls to mock objects are indexed chronologically and both `mock_call()`
#' and `mock_args()` provide an argument `call_no` which can be used to specify
#' which call is of interest, with the default being the most recent (or last)
#' one.
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
#' dl <- function(x) curl::curl(x)
#'
#' with_mock(`curl::curl` = mk, dl(url))
#'
#' mock_call(mk)
#' mock_args(mk)
#'
#' local({
#'   mk <- local_mock(`curl::curl` = "mocked request")
#'   list(dl(url), mock_arg(mk, "url"))
#' })
#'
#' @return
#' * `local_mock()`: the result of the last unnamed argument passed as `...`
#'   (evaluated in the environment passed as `eval_env`)
#' * `local_mock()`: a list of functions or `mock_fun` objects (invisibly)
#' * `mock()`: a `mock_fun` object
#' * `mock_call()`: a call (created by [base::match.call()])
#' * `mock_arg()`: the object used as specified function argument
#' * `mock_args()`: a list of all function arguments used to create a call to
#'   the `mock_fun` object in question
#' * `mock_n_called()`: a scalar integer
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
#' @param call_no The call number of interest (in case the function was called
#' multiple times).
#'
#' @rdname mock
#' @export
mock_call <- function(x, call_no = mock_n_called(x)) {

  stopifnot(length(call_no) == 1L, is.numeric(call_no))

  get("call", envir = attr(singleton_list_to_mocked_fun(x), "env"))[[call_no]]
}

#' @rdname mock
#' @export
mock_args <- function(x, call_no = mock_n_called(x)) {
  mock_arg_retriever(x, NULL, call_no)
}

#' @param arg String-valued argument name to be retrieved.
#'
#' @rdname mock
#' @export
mock_arg <- function(x, arg, call_no = mock_n_called(x)) {
  stopifnot(is.character(arg), length(arg) == 1L)
  mock_arg_retriever(x, arg, call_no)
}

mock_arg_retriever <- function(x, arg, call_no) {

  stopifnot(length(call_no) == 1L, is.numeric(call_no))

  x <- singleton_list_to_mocked_fun(x)

  env <- attr(x, "env")
  fun <- get("fun", envir = env)

  called_args <- get("args", envir = env)[[call_no]]
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

#' @rdname mock
#' @export
mock_n_called <- function(x) {
  length(get("call", envir = attr(singleton_list_to_mocked_fun(x), "env")))
}

singleton_list_to_mocked_fun <- function(x) {

  if (is.list(x) && length(x) == 1L) {
    x <- x[[1L]]
  }

  stopifnot(is_mock_fun(x))

  x
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
    mcal  <- match.call()
    call <<- c(call, mcal)
    args <<- c(args, list(lapply(as.list(mcal)[-1L], eval, parent.frame())))
  })

  if (is.function(quo)) {
    par <- environment(quo)
    quo <- body(quo)
  } else {
    par <- env
  }

  env <- list2env(list(call = list(), args = list()), parent = par)

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

  fun_exists <- function(name, envir) {
    is.environment(envir) &&
      exists(name, envir = envir, mode = "function", inherits = FALSE)
  }


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

  if (fun_exists(fun_name, env) && !fun_exists(fun_name, environment(fun))) {
    new_mock(fun_name, fun, new_val, env)
  } else if (fun_exists(fun_name, imports_env(env))) {
    new_mock(fun_name, fun, new_val, imports_env(env))
  } else {
    new_mock(fun_name, fun, new_val, environment(fun))
  }
}

imports_env <- function(x) {

  if (!isNamespace(x)) {
    return(NULL)
  }

  res <- parent.env(x)
  nme <- paste("imports", environmentName(x), sep =":")

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
