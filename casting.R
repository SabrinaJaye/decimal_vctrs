## Casting for decimal_vctr ##

box::use(
  vctrs[
    vec_cast
  ]
)

# A) decimal_vctr --------------------------------------------------------------

# 1. decimal_vctr to decimal_vctr ----------------------------------------------

#' @export
vec_cast.decimal_vctr.decimal_vctr <- function(x, to, ...) x


# 2. Cast to and from compatible types -----------------------------------------

# a) decimal_vctr to character

#' @export
vec_cast.character.decimal_vctr <- function(x, to, ...) {
  format(x, ...)
}

box::register_S3_method(
  "vec_cast",
  "character.decimal_vctr",
  vec_cast.character.decimal_vctr
)