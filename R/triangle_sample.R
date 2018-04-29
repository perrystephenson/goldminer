#' Generate random data from a triangular distribution
#'
#' @param n integer, number of random samples
#' @param a float, the minimum value for the distribution
#' @param b float, the maximum value for the distribution
#' @param c float, the most likely value for the distribution
#'
#' @return a vector of floats, drawn from the triangular distribution
#'
#' @examples
#' # Generate 10 samples from a triangular distribution with:
#' # - minimum value 1
#' # - maximum value 5
#' # - expected value 2.2
#'
#' triangle_sample(10,1,5,2.2)
#'
#' @export
triangle_sample <- function(n, a, b, c) {

  # Generate some data from the uniform distribution
  U <- runif(n, min = 0, max = 1)

  # Use the inverse CDF to find the corresponding point from the triangular distribution
  Fc <- (c-a)/(b-a)
  dplyr::case_when(
    (U > 0 & U < Fc) ~ a + sqrt(U * (b-a) * (c-a)),
    (U >= Fc & U < 1) ~ b - sqrt((1-U) * (b-a) * (b-c))
  )
}
