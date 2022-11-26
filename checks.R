## Checks ##

box::use(
  cli[cli_abort],
  rlang[are_na, caller_arg, caller_env],
  vctrs[vec_size]
)

# numeric check ----------------------------------------------------------------

numeric_check <- function(x, arg = caller_arg(x), call = caller_env()) {
  if(!all(are_na(x))) {
    if(!is.numeric(x)) {
      cli_abort(
        c(
          "{.arg {arg}} must be a numeric vector.",
          "x" = "You've supplied a {.cls {class(x)}}."
        ),
        call = call
      )
    }
  }
}

logical_check <- function(x, arg = caller_arg(x), call = caller_env()) {
  if(!all(are_na(x))) {
    if(!is.logical(x)) {
      cli_abort(
        c(
          "{.arg {arg}} must be a logical vector.",
          "x" = "You've suplied a {.cls {class(x)}}."
        )
      )
    }
  }
}
  
# scq checks -------------------------------------------------------------------

#' Checks for ait_decimal functions
#'
#' @description
#' Checks made:
#' - That `s` is logical
#' - That `c`, and `q` are integer
#' - That they are the same length, length 1, or all length 0
#' @keywords internal
#' @export
check_scq <- function(s, c, q, call = caller_env()) {
  # Check the types of s, c, and q
  logical_check(s, call = call)
  numeric_check(c, call = call)
  numeric_check(q, call = call)
  
  # Check that s, c, and q are same length, length 1, or all length 0.
  lengths <- c(vec_size(s), vec_size(c), vec_size(q))
  
  # Must be either all zero length or no zero length
  if (!all(lengths == 0L) || any(lengths == 0L)) {
    missing <- c("`s`", "`c`", "`q`")[lengths == 0]
    cli_abort(
      c(
        "{missing} {?is/are} absent but must be supplied.",
        "x" = "`s`, `c`, and `q` must all have values.",
        "i" = "You may have forgotten a value or need to use 0."
      ),
      call = call
    )
  }
  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if(vec_unique_count(non_scalar) > 1L) {
    cli_abort(
      c(
        "`s`, `c`, and `q` must have compatible lengths",
        "i" = "Only values of length one are recycled.",
        "*" = "Unit `s` has length {lengths[[1]]}.",
        "*" = "Unit `c` has length {lengths[[2]]}.",
        "*" = "Unit `q` has length {lengths[[3]]}."
      ),
      call = call
    )
  }
  
}