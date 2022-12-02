
#' @export
box::use(
  ./class[
    decimal,
    is_decimal_vctr
  ],
  ./coercion,
  ./casting
)

# This allows you to run the tests by running the module source from the command
# line, e.g. `Rscript decimal_vctrs/__init__.R`
if(is.null(box::name())) {
  box::use(./tests)
}