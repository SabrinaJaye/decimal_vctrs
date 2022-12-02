box::use(
  vctrs[vec_cast, vec_ptype2, vec_init, vec_recycle, vec_size, vec_slice, `vec_slice<-`]
)

#' @export
trailing_zeros <- function(x) {
  out <- vec_init(x, vec_size(x))
  non_zero <- x != 0L
  nz_x <- vec_slice(x, non_zero)
  trailing <- vec_recycle(0L, vec_size(nz_x))
  update <- (nz_x %% 10L ^ (trailing + 1L) == 0L)
  while(any(update)) {
    trailing <- trailing + update
    update <- (nz_x %% 10L ^ (trailing + 1L) == 0L)
  }
  vec_slice(out, non_zero) <- trailing
  vec_slice(out, !non_zero) <- 0L
  return(out)
}

#'
#' @keywords internal
#' @export
normalize_scq <- function(s, c, q, rounded = FALSE) {
  tz <- trailing_zeros(c)
  new_c <- c %/% 10L ^ tz
  new_q <- q + tz
  new_q[new_c == 0L & !s] <- 0L
  
  list(s = s, c = new_c, q = new_q)
}
