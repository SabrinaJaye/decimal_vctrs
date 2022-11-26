## Checks ##

box::use(
  cli[cli_abort],
  rlang[are_na, caller_arg, caller_env],
  vctrs[vec_size]
)

# numeric check ----------------------------------------------------------------

numeric_check <- function(x, arg = caller_arg(x), call = caller_env()) {
  if(!all(are_na(x))){
    if(!is.numeric(x)) {
      cli_abort(
        c(
          "{.arg {arg}} must be a numeric vector.",
          "x" = "You've supplied a {.cls {class(x)}} vector."
        ),
        call = call
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
  if (!all(are_na(s))) {
    if (!is.logical(s)) {
      stop(call. = FALSE, "`s` must be a logical vector.")
    }
  }
  
  numeric_check(c, call = call)
  numeric_check(q, call = call)
  
  # Check that s, c, and q are same length, length 1, or all length 0.
  lengths <- c(vec_size(s), vec_size(c), vec_size(q))
  # Must be either all zero length or no zero length
  if (sum(lengths) == 1L || sum(lengths) == 2L) {
    stop(call. = FALSE,
         paste0("`s`, `c`, and `q` must all have values. ",
                "You may have forgotten a value or need to use 0."))
  }
  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if (length(unique(non_scalar)) > 1L) {
    stop(call. = FALSE,
         "`s`, `c`, and `q` must be vectors of equal length or length 1.")
  }
  
}