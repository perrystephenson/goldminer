#' Generate leased mine profit simulation values
#'
#' @param rounds integer, number of simulations
#' @param years integer, the number of years of data to generate (min 2)
#' @param profit_init list, the parameters for a triangular distribution
#' @param profit_delta list, the parameters for a triangular distribution
#'
#' @return a data frame with dimensions _(rounds,years)_, with simulated leased
#'     mine profit values
#'
#' @examples
#' # Generate 10,000 simulations for 5 years:
#'
#' gen_lease_profit(10000, 5, list(a = 1000000, b = 3000000, c = 2000000),
#'                            list(a = 0.8, b = 1.45, c = 1.1))
#'
#' @export
gen_lease_profit <- function(rounds, years, profit_init, profit_delta) {
  Year1 <- do.call(triangle_sample, c(rounds, profit_init))
  lease_profit <- tibble::tibble(Year1)
  for (i in 2:years) {
    profit1 <- lease_profit[[1]]
    rand <- do.call(triangle_sample, c(rounds, profit_delta))
    this_year <- profit1 * rand
    lease_profit[[paste0("Year", i)]] <- this_year
  }
  return(lease_profit)
}
