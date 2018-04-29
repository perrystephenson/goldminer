#' Generate a table of independent simulation values
#'
#' @param rounds integer, number of simulations
#' @param years integer, the number of years of data to generate (min 2)
#' @param t list, the parameters for the triangular distribution
#'
#' @return a data frame with dimensions _(rounds,years)_, with simulated
#'     independent values given by **t**.
#'
#' @examples
#' # Generate 10,000 simulations for 5 years:
#'
#' gen_independent(10000, 5, list(a = 0.08, b = 0.16, c = 0.1275))
#'
#' @export
gen_independent <- function(rounds, years, t) {
  Year1 <- do.call(triangle_sample, c(rounds, t))
  output <- tibble::tibble(Year1)
  for (i in 2:years) {
    output[[paste0("Year", i)]] <- do.call(triangle_sample, c(rounds, t))
  }
  return(output)
}
