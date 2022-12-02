## Casting for decimal_vctr ##

box::use(
  readr[parse_integer],
  vctrs[
    vec_cast
  ],
  ./class[decimal]
)

# A) decimal_vctr --------------------------------------------------------------

# 1. decimal_vctr to decimal_vctr ----------------------------------------------

#' @export
vec_cast.decimal_vctr.decimal_vctr <- function(x, to, ...) x


# 2. Cast to and from compatible types -----------------------------------------

# a) decimal_vctr & character

#' @export
vec_cast.character.decimal_vctr <- function(x, to, ...) {
  format(x, ...)
}

box::register_S3_method(
  "vec_cast",
  "character.decimal_vctr",
  vec_cast.character.decimal_vctr
)


# b) decimal_vctr & integer

#' @export
vec_cast.integer.decimal_vctr <- function(x, to, ...) {
  parse_integer(format(x))
}

box::register_S3_method(
  "vec_cast",
  "integer.decimal_vctr",
  vec_cast.integer.decimal_vctr
)


#' @export
vec_cast.decimal_vctr.integer <- function(x, to, ...) {
  decimal(x < 0, x, 0L)
}

box::register_S3_method(
  "vec_cast",
  "decimal_vctr.integer",
  vec_cast.decimal_vctr.integer
)


# b) decimal_vctr & logical

#' @export
vec_cast.decimal_vctr.logical <- function(x, to, ...){
  decimal(FALSE, vec_cast(x, integer()), 0L)
}

box::register_S3_method(
  "vec_cast",
  "decimal_vctr.logical",
  vec_cast.decimal_vctr.logical
)