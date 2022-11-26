## Coercion for decimal_vctr prototypes ##

## What should the type hierarchy be?
#
# - decimal_vctr can be considered a subtype of character: character can contain
#   a representation of decimal_vctr and other unrelated string values too.

box::use(
  vctrs[vec_ptype2],
  
  ./class[new_decimal]
)

# 1. ait_decimal and ait_decimal -----------------------------------------------

#' @export
vec_ptype2.decimal_vctr.decimal_vctr <- function(x, y, ...) {
  new_decimal()
}

box::register_S3_method(
  "vec_ptype2",
  "decimal_vctr.decimal_vctr",
  vec_ptype2.decimal_vctr.decimal_vctr
)


# 2. Coercion with compatible types --------------------------------------------

# a) character -----------------------------------------------------------------  

#' @export
vec_ptype2.decimal_vctr.character <- function(x, y, ...) y

#' @export
vec_ptype2.character.decimal_vctr <- function(x, y, ...) x

box::register_S3_method(
  "vec_ptype2",
  "decimal_vctr.character",
  vec_ptype2.decimal_vctr.character
)

box::register_S3_method(
  "vec_ptype2",
  "character.decimal_vctr",
  vec_ptype2.character.decimal_vctr
)