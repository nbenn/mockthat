
#' Mocking helper functions
#'
#' Calls to mock-objects either constructed using `mock()` or returned by
#' `local_mock()` can keep track of how they were called and functions
#' `mock_call()`, `mock_arg/s()` and `mock_n_called()` can be used to retrieve
#' related information.
#'
#' @details
#' A mocking function can be created either from a single object to be used
#' as return value or from an expression which is used as function body. In
#' both cases, the function signature is inferred from the mock-target.
#' Furthermore, closures constructed by `mock()` are able to keep track of
#' call objects and arguments passed to their respective targets. The
#' following utility functions are available to query this information:
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
#' @param expr Expression to be used as body of the function to be mocked.
#' @param env Environment used as ancestor to the mock function environment.
#'
#' @examples
#' url <- "https://eu.httpbin.org/get?foo=123"
#'
#' mk <- mock("mocked request")
#' dl <- function(x) curl::curl(x)
#'
#' with_mock(`curl::curl` = mk, dl(url))
#'
#' mock_call(mk)
#' mock_args(mk)
#' mock_n_called(mk)
#'
#' mk <- mock({
#'   url
#' })
#'
#' with_mock(`curl::curl` = mk, dl(url))
#'
#' my_return_val <- "mocked request"
#' mk <- mock(my_return_val)
#'
#' with_mock(`curl::curl` = mk, dl(url))
#'
#' @return
#' * `mock()`: a `mock_fun` object
#' * `mock_call()`: a call (created by [base::match.call()])
#' * `mock_arg()`: the object used as specified function argument
#' * `mock_args()`: a list of all function arguments used to create a call to
#'   the `mock_fun` object in question
#' * `mock_n_called()`: a scalar integer
#'
#' @rdname helpers
#' @export
mock <- function(expr, env = parent.frame()) {
  mock_quo(substitute(expr), env = env)
}

#' @param x Object of class `mock_fun` to be queried for call and argument
#' information.
#' @param call_no The call number of interest (in case the function was called
#' multiple times).
#'
#' @rdname helpers
#' @export
mock_call <- function(x, call_no = mock_n_called(x)) {

  stopifnot(length(call_no) == 1L, is.numeric(call_no))

  get("call", envir = attr(singleton_list_to_mocked_fun(x), "env"))[[call_no]]
}

#' @rdname helpers
#' @export
mock_args <- function(x, call_no = mock_n_called(x)) {
  mock_arg_retriever(x, NULL, call_no)
}

#' @param arg String-valued argument name to be retrieved.
#'
#' @rdname helpers
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

  if ("..." %in% names(formal_args)) {
    formal_args <- formal_args[!names(formal_args) %in% "..."]
  }

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

#' @rdname helpers
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

  if (is.language(quo) && !is.symbol(quo) &&
      identical(quo[[1L]], quote(`{`))) {

    quo <- quo[-1L]

  } else if (is.language(quo)) {

    quo <- as.expression(quo)

  } else {

    quo <- list(quo)
  }

  capt[seq_along(quo) + length(capt)] <- quo

  structure(capt, env = env, class = "mock_fun")
}
