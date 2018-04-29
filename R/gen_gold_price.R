#' Generate gold price simulation values
#'
#' @param rounds integer, number of simulations
#' @param years integer, the number of years of data to generate (min 2)
#' @param gold_price_init int, the value of gold in the first year
#' @param gold_price_delta list, the parameters for the triangular distribution
#'
#' @return a data frame with dimensions _(rounds,years)_, with simulated gold
#'     price values
#'
#' @examples
#' # Generate 10,000 simulations for 5 years:
#'
#' gen_gold_price(10000, 5, 1700, list(a = -100, b = 500, c = 100))
#'
#' @export
gen_gold_price <- function(rounds, years, gold_price_init, gold_price_delta) {
  Year1 <- rep(gold_price_init, rounds)
  gold_price <- tibble::tibble(Year1)
  for (i in 2:years) {
    prev_year <- gold_price[[i-1]]
    rand <- do.call(triangle_sample, c(rounds, gold_price_delta))
    this_year <- prev_year + rand
    gold_price[[paste0("Year", i)]] <- this_year
  }
  return(gold_price)
}
