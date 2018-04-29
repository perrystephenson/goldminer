#' Generate reputation management cost simulation values
#'
#' @param rounds integer, number of simulations
#' @param years integer, the number of years of data to generate (min 2)
#' @param fixed int, fixed annual retainer for reputation management
#' @param rate int, fixed daily cost for reputation management work
#' @param hours list, the parameters for the triangular distribution
#'
#' @return a data frame with dimensions _(rounds,years)_, with simulated
#'     reputation management cost values
#'
#' @examples
#' # Generate 10,000 simulations for 5 years:
#'
#' gen_rep_mgmt(10000, 5, 350000, 2500, list(a = 75, b = 350, c = 125))
#'
#' @export
gen_rep_mgmt <- function(rounds, years, fixed, rate, hours) {
  rep_mgmt <- tibble::tibble("Year1" = rep(NA, rounds))
  for (i in 1:years) {
    rand <- do.call(triangle_sample, c(rounds, hours))
    this_year <- rand * rate + fixed
    rep_mgmt[[paste0("Year", i)]] <- this_year
  }
  return(rep_mgmt)
}
