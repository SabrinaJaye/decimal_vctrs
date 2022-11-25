## Define the ait_decimal class ##

# The ait_decimal class is based on a record style vector.
# Underneath it is a list of three equal-length vectors, one logical and two integers.

box::use(
  # Packages
  rlang[are_na],
  vctrs[
    field,
    new_rcrd,
    vec_assert,
    vec_cast,
    vec_cast_common,
    vec_recycle_common,
    vec_size,
    vec_ptype_abbr
  ],
  zeallot[`%<-%`],
  
  # Local modules
  ./checks[check_scq]
)

# 1. Constructor ---------------------------------------------------------------

#' Internal constructor to create the ait_decimal type
#' 
#' Asserts that `s` is aof type `logical()`, and that `c`, and `q` are of type
#' `integer()`. Creates the object through use of `vctrs::new_rcrd()`.
#' 
#' @keywords internal
#' @export
new_decimal <- function(s = logical(), c = integer(), q = integer()) {
  # Assert that s, c, and q are of the correct type.
  vec_assert(s, ptype = logical())
  vec_assert(c, ptype = integer())
  vec_assert(q, ptype = integer())
  
  # Create the class
  new_rcrd(list(s = s, c = c, q = q), class = "ait_decimal")
}


# 2. Helper Constructor --------------------------------------------------------

#' A class for arbitrary precision decimal floating point values and arithmetic
#' 
#' User-facing function to create a ait_decimal vector.
#' @param s Logical vector indicating the signs.
#' @param c Integer vector representing the coefficients.
#' @param q Integer vector representing the quotients.
#' @export
#' @examples
#' 
#' ait_decimal(TRUE, 87, -3)
ait_decimal <- function(s = logical(), c = integer(), q = integer()) {
  check_scq(s, c, q)
  
  # Cast for compatible types
  c(c, q) %<-% vec_cast_common(c, q, .to = integer())
  s <- vec_cast(s, to = logical())
  
  # Enforce recycling rules
  c(s, c, q) %<-% vec_recycle_common(s, c, q)
  
  
  
  # Create ait_decimal vector
  new_decimal(s = s, c = c, q = q)
}


# 3. Formally declare S3 class -------------------------------------------------

# This is actually not possible in box because of how S4 works only in global
# namespace or packages. But that's fine, we can stick to S3.
#
# setOldClass(c("ait_decimal", "vctrs_rcrd", "vctrs_vctr"))


# 4. Attribute access ----------------------------------------------------------

#' Access the `units` attribute of an `ait_decimal` object.
#'
#' @keywords internal
ait_dec_units <- function(x) attr(x, "units")


# 5. Class check ---------------------------------------------------------------

#' Test if an object is of class `ait_decimal`
#' @param x An object.
#' 
#' @return `TRUE` if object is of class `ait_decimal` and `FALSE` if it is not.
#' @export
is_ait_decimal <- function(x) inherits(x, "ait_decimal")


# 6. Format method -------------------------------------------------------------

#' ait_decimal format method for object printing
#' 
#' @keywords internal
#' @export
format.ait_decimal <- function(x, ...) {
  x_valid <- which(!is.na(x))
  
  s <- ifelse(field(x, "s"), "-", "")
  c <- field(x, "c")
  q <- field(x, "q")
  
  out <- paste0(s, c, "e", q)
  out[are_na(s) | are_na(c) | are_na(q)] <- NA
  
  out
}

box::register_S3_method("format", "ait_decimal", format.ait_decimal)


# 7. Abbreviated name type -----------------------------------------------------
# Used in column labels in tibble and str()

#' Abbreviated name for tibble columns
#' 
#' @keywords internal
#' @export
vec_ptype_abbr.ait_decimal <- function(x) { "ait_decimal" }

box::register_S3_method("vec_ptype_abbr", "ait_decimal", vec_ptype_abbr.ait_decimal)
