#' Calculate profits from a mine environment where actions are taken, and search
#' starts immediately
#'
#' @param env1 list, environment simulation for mines 1 and 2
#' @param env3 list, environment simulation for mines 3
#' @param params list, mining simulation parameters
#' @param sell_m2 integer, 1-5, the year in which the mine is sold
#' @param search integer, 0-6, the number of search parties to send
#'
#' @return a list with data frames containing various profit metrics
#'
#' @examples
#'
#' profits_with_search(env1, env3, 2, 6)
#'
#' @export
profits_with_search <- function(env1, env3, params, sell_m2=0, search=0) {

  # Calculate the annual figures for the first mine
  revenue <- env1$gold_prod * env1$gold_price
  costs <- (env1$selling_cost * revenue) + env1$equip_labour
  profit <- revenue - costs

  # Calculate the annual figures for the second mine, accounting for the sale
  leased_profit <- env1$lease_profit - env1$rep_mgmt
  if(sell_m2 %in% 1:5) {
    for (i in sell_m2:5) {
      leased_profit[,i] <- 0
    }
    leased_profit[,sell_m2] <- params$sale_price[[sell_m2]]
  }

  # Add these figures to the profit
  total_profit <- profit + leased_profit

  # Make some empty tibbles and stuff
  search_costs <- env1$gold_price              # Make a copy for shape
  search_costs[,] <- 0                         # Set copy to zero
  mine_found <- rep(FALSE, nrow(search_costs)) # Boolean - will update in loop
  gold_prod_m3 <- search_costs                 # Another zero copy

  # Deal with the mine search
  for (i in 1:4) {
    # Update production for mines found last round
    gold_prod_m3[mine_found,i:5] <- env3$gold_prod[mine_found, 1:(6-i)]

    # Update costs for simulations still looking
    search_price <- params$search_price[[search]]
    search_costs[!mine_found,i] <- search_price

    # See which sims find it this round
    search_rand <- runif(sum(!mine_found))
    mine_found[!mine_found] <- search_rand <= params$search_prob[[search]]
  }

  # gold_prod_m3 now has mine production, taking search uncertainty into account
  # Also need to create a "mine_operational" boolean data frame
  m3_operational <- gold_prod_m3 > 0

  # Calculate the annual figures for the third mine
  m3_revenue <- gold_prod_m3 * env3$gold_price
  m3_costs   <- (env3$selling_cost * m3_revenue) + (m3_operational * env3$equip_labour)
  m3_profit  <- m3_revenue - m3_costs - search_costs

  # Calculate total profit
  total_profit <- total_profit + m3_profit

  # Calculate the cash on hand, and interest charges

  # Mainly making a copy to overwrite, but the first column is correct
  cash <- total_profit

  # Calculate interest charges
  interest_charge <- env1$interest_rate * cash
  interest_charge[interest_charge > 0] <- 0

  # Calculate interest earnings
  interest_earnings <- (env1$interest_rate - 0.05) * cash
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
    interest_charge[[i]] <- env1$interest_rate[[i]] * cash[[i]]
    pos_rows <- interest_charge[[i]] > 0
    interest_charge[pos_rows, i] <- 0
    interest_earnings[[i]] <- (env1$interest_rate[[i]] - 0.05) * cash[[i]]
    neg_rows <- interest_charge[[i]] < 0
    interest_charge[neg_rows, i] <- 0
    interest[[i]] <- interest_charge[[i]] + interest_earnings[i]
  }

  # Summarise financial position
  rounds <- nrow(env1$gold_prod) # Just getting the number of simulations
  fin_pos <- tibble::tibble(
    "Year1" = rep("Loss", rounds),
    "Year2" = rep("Loss", rounds),
    "Year3" = rep("Loss", rounds),
    "Year4" = rep("Loss", rounds),
    "Year5" = rep("Loss", rounds)
  )
  fin_pos[cash > 0] <- "Profit"
  fin_pos[cash < -4e6L] <- "Foreclosure"

  return(list("profit" = total_profit,
              "cash" = cash,
              "interest" = interest,
              "financial_position" = fin_pos))
}
