s <- c(T, F, F, F, F)
c <- c(87L, 1L, 1L, 75L, 1L)
q <- c(-3L, 1L, 11L, 1L, 5L)
c("-8.7e-2", "10", "10e10", "7.5e2", "100000")

test_that(
  "decimal_vctrs can be cast to character vectors", {
    expect_true(
      all(vec_equal(
        vec_cast(decimal(s, c, q), character()),
        c("-0.087", "10", "100000000000", "750", "100000")
      ))
    )
  }
)