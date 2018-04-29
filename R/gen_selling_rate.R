#' Generate selling rate simulation values
#'
#' @param rounds integer, number of simulations
#' @param years integer, the number of years of data to generate (min 2)
#' @param cost_init list, the parameters for a triangular distribution
#' @param cost_delta list, the parameters for a triangular distribution
#'
#' @return a data frame with dimensions _(rounds,years)_, with simulated selling
#'     cost rate values
#'
#' @examples
#' # Generate 10,000 simulations for 5 years:
#'
#' gen_selling_rate(10000, 5, list(a = 0.05, b = 0.095, c = 0.07),
#'                            list(a = -0.1, b = 0.45, c = 0.2275))
#'
#' @export
gen_selling_rate <- function(rounds, years, cost_init, cost_delta) {
  Year1 <- do.call(triangle_sample, c(rounds, cost_init))
  selling_cost <- tibble::tibble(Year1)
  for (i in 2:years) {
    rand <- do.call(triangle_sample, c(rounds, cost_delta))
    last_year <- selling_cost[[i-1]]
    this_year <- last_year * (1 + rand)
    selling_cost[[paste0("Year", i)]] <- this_year
  }
  return(selling_cost)
}

