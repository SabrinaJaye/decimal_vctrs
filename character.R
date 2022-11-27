
box::use(
  rlang[are_na],
  stringr[str_sub, str_dup],
  vctrs[vec_assert, vec_cast, vec_size]
)

logical_to_sign <- function(x) {
  ifelse(vec_cast(x, logical()), "-", "")
}

write_q_pos_character <- function(s, c, q) {
  paste0(s, c, str_dup("0", q))
}

write_q_neg_character <- function(s, c, q) {
  out <- character(vec_size(s))
  
  shift <- q + nchar(c)
  
  p <- shift > 0L
  ps <- s[p]
  pc <- c[p]
  pq <- q[p]
  pshift <- shift[p]
  p_out <- paste0(
    ps, str_sub(pc, end = pshift), ".", str_sub(pc, start = pshift + 1L)
  )
  
  n <- !p
  ns <- s[n]
  nc <- c[n]
  nq <- q[n]
  nshift <- shift[n]
  n_out <- paste0(ns, "0.", str_dup("0", -nshift), nc)
  
  out[p] <- p_out
  out[n] <- n_out
  out
}


#' @keywords internal
#' @export
scq_as_decimal_character <- function(s, c, q) {
  s <- logical_to_sign(s)
  c <- vec_cast(c, integer())
  q <- vec_cast(q, integer())
  
  out <- character(vec_size(s))
  
  pos_q <- q > 0L
  neg_q <- q < 0L
  zero_q <- !(pos_q | neg_q)
  
  out[pos_q] <- write_q_pos_character(s[pos_q], c[pos_q], q[pos_q])
  out[neg_q] <- write_q_neg_character(s[neg_q], c[neg_q], q[neg_q])
  out[zero_q] <- paste0(s[zero_q], c[zero_q])
  
  out
}


#' @keywords internal
#' @export
scq_as_scinote_character <- function(s, c, q) {
  s <- logical_to_sign(s)
  
  out <- paste0(s, c, "e", q)
  out[are_na(s) | are_na(c) | are_na(q)] <- NA
  out
}


#  Convert a decimal to a string
# function Base.print(io::IO, x::Decimal)
# c = string(x.c)
# negative = (x.s == 1) ? "-" : ""
# if x.q > 0
# print(io, negative, c, repeat("0", x.q))
# elseif x.q < 0
# shift = x.q + length(c)
# if shift > 0
# print(io, negative, c[1:shift], ".", c[(shift+1):end])
# else
#   print(io, negative, "0.", repeat("0", -shift), c)
# end
# else
#   print(io, negative, c)
# end
# end


















is_scientific_notation <- function(x) {
  vec_assert(x, ptype = character())
  str_detect(x, "-?[\\d[.]]e-?\\d+")
}


#' @keywords internal
#' @export
parse_dec_to_scq <- function(x) {
  vec_assert(x, ptype = character())
  
  is_sci_notation <- 
  x[is_sci_notation] <- parse_sci_to_dec(x[is_sci_notation])
  
  s <- str_detect(x, "^-")
  c <- as.integer(str_remove(str_remove_all(x, "[.]|-"), "^0+"))
  q <- map_int(str_split(str, "[.]"), function(x) -nchar(x[2]))
  q[!str_detect(str, "[.]")] <- 0L
  
  list(s = s, c = c, q = q)
}

parse_sci_to_dec <- function(x) {
  # Split strings into signs, numbers, and exponents. 
  signs <- if_else(str_detect(x, "^-"), "-", "")
  n_expo <- str_split(x, "e")
  n <- map(n_expo, function(x) {
    str_replace(unlist(str_split(x[[1]], "[.]")), "-", "")
  })
  expo <- as.integer(map_chr(n_expo, function(x) x[2]))
  
  # Calculate the shift required to pad zeros. This is done in as vectorized way
  # as I could manage.
  n_len <- map_int(n, function(x) length(x))
  len_two <- n_len == 2
  pos_expo <- expo > 0
  p_shift <- expo - if_else(len_two, map_int(n, function(x) nchar(x[2])), 0L)
  n_shift <- -expo - if_else(len_two, map_int(n, function(x) nchar(x[1])), n_len)
  shift <- if_else(pos_expo, p_shift, n_shift)
  
  n_join <- map_chr(n, function(x) paste0(x, collapse = ""))
  
  a <- ifelse(pos_expo, n_join, "0.")
  b <- strrep("0", shift)
  c <- ifelse(pos_expo, "", n_join)
  
  paste0(signs, a, b, c)
}
