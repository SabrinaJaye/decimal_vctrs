## Define the decimal_vctr class ##

# The decimal_vctr class is based on a record style vector.
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
  ./checks[check_scq],
  ./character[scq_as_decimal_character]
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
  new_rcrd(list(s = s, c = c, q = q), class = "decimal_vctr")
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
decimal <- function(s = logical(), c = integer(), q = integer()) {
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
# setOldClass(c("decimal_vctr", "vctrs_rcrd", "vctrs_vctr"))


# 4. Attribute access ----------------------------------------------------------




# 5. Class check ---------------------------------------------------------------

#' Test if an object is of class `decimal_vctr`
#' @param x An object.
#' 
#' @return `TRUE` if object is of class `decimal_vctr` and `FALSE` if it is not.
#' @export
is_decimal_vctr <- function(x) inherits(x, "decimal_vctr")


# 6. Format method -------------------------------------------------------------

#' ait_decimal format method for object printing
#' 
#' @keywords internal
#' @export
format.decimal_vctr <- function(x, ...) {
  scq_as_decimal_character(field(x, "s"), field(x, "c"), field(x, "q"))
}

box::register_S3_method("format", "decimal_vctr", format.decimal_vctr)


# 7. Abbreviated name type -----------------------------------------------------
# Used in column labels in tibble and str()

#' Abbreviated name for tibble columns
#' 
#' @keywords internal
#' @export
vec_ptype_abbr.decimal_vctr <- function(x) { "decimal_vctr" }

box::register_S3_method(
  "vec_ptype_abbr",
  "decimal_vctr",
  vec_ptype_abbr.decimal_vctr
)
