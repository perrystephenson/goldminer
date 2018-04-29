#' Generate gold production simulation values
#'
#' @param rounds integer, number of simulations
#' @param years integer, the number of years of data to generate (min 2)
#' @param prod_init list, the parameters for a triangular distribution
#' @param prod_delta list, the parameters for a triangular distribution
#'
#' @return a data frame with dimensions _(rounds,years)_, with simulated gold
#'     production values
#'
#' @examples
#' # Generate 10,000 simulations for 5 years:
#'
#' gen_gold_prod(10000, 5, list(a = 300, b = 1400, c = 500),
#'                         list(a = 0.8, b = 1.8, c = 1.4))
#'
#' @export
gen_gold_prod <- function(rounds, years, prod_init, prod_delta) {
  Year1 <- do.call(triangle_sample, c(rounds, prod_init))
  gold_prod <- tibble::tibble(Year1)
  for (i in 2:years) {
    prev_year <- gold_prod[[i-1]]
    this_year <- prev_year * do.call(triangle_sample, c(rounds, prod_delta))
    gold_prod[[paste0("Year", i)]] <- this_year
  }
  return(gold_prod)
}
