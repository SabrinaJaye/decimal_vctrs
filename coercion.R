## Coercion for ait_decimal prototypes ##

box::use(
  vctrs[],
  
)

# 1. ait_decimal and ait_decimal -----------------------------------------------

#' @export
vec_ptype2.ait_decimal.ait_decimal <- function(x, y, ...) {
  new_decimal()
}