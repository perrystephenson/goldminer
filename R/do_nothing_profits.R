#' Calculate profits from a mine environment without taking any actions
#'
#' @param env list, a mining environment
#'
#' @return a list with data frames containing various profit metrics
#'
#' @examples
#'
#' do_nothing_profits(env1)
#'
#' @export
do_nothing_profits <- function(env) {

  # Calculate the annual figures
  revenue <- env$gold_prod * env$gold_price
  costs <- (env$selling_cost * revenue) + env$equip_labour
  profit <- revenue - costs
  leased_profit <- env$lease_profit - env$rep_mgmt
  total_profit <- profit + leased_profit

  # Calculate the cash on hand, and interest charges

  # Mainly making a copy to overwrite, but the first column is correct
  cash <- total_profit

  # Calculate interest charges
  interest_charge <- env$interest_rate * cash
  interest_charge[interest_charge > 0] <- 0

  # Calculate interest earnings
  interest_earnings <- (env$interest_rate - 0.05) * cash
  interest_earnings[interest_earnings < 0] <- 0

  # Calculate total interest
  interest <- interest_charge + interest_earnings

  for (i in 2:ncol(cash)) {
    # Update the cash for this year
    cash[[i]] <- cash[[i-1]] + interest[[i-1]] + total_profit[[i]]

    # Manually reset the cash to last year's value if < 4 million negative
    broke_rows <- cash[[i-1]] < -4e6L
    cash[broke_rows, i] <- cash[broke_rows, i-1]

    # Calculate interest on the cash balance (split pos and neg)
    interest_charge[[i]] <- env$interest_rate[[i]] * cash[[i]]
    pos_rows <- interest_charge[[i]] > 0
    interest_charge[pos_rows, i] <- 0
    interest_earnings[[i]] <- (env$interest_rate[[i]] - 0.05) * cash[[i]]
    neg_rows <- interest_charge[[i]] < 0
    interest_charge[neg_rows, i] <- 0
    interest[[i]] <- interest_charge[[i]] + interest_earnings[i]
  }

  # Summarise financial position
  rounds <- nrow(env$gold_prod)
  fin_pos <- tibble::tibble(
    "Year1" = rep("Loss", rounds),
    "Year2" = rep("Loss", rounds),
    "Year3" = rep("Loss", rounds),
    "Year4" = rep("Loss", rounds),
    "Year5" = rep("Loss", rounds)
  )
  fin_pos[cash > 0] <- "Profit"
  fin_pos[cash < -4e6L] <- "Foreclosure"

  return(list("revenue" = revenue,
              "costs" = costs,
              "profit" = profit,
              "leased_profit" = leased_profit,
              "total_profit" = total_profit,
              "cash" = cash,
              "interest" = interest,
              "financial_position" = fin_pos))
}
