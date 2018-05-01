#' Calculate profits from a mine environment where actions are taken
#'
#' @param env1 list, environment simulation for mines 1 and 2
#' @param env3 list, environment simulation for mines 3
#' @param params list, mining simulation parameters
#' @param sell_m2 integer, 1-5, the year in which the mine is sold
#' @param find_m3 integer, 1-4, the year in which the third mine is found
#'
#' @return a list with data frames containing various profit metrics
#'
#' @examples
#'
#' profits_with_actions(env1, env3, 2, 3)
#'
#' @export
profits_with_actions <- function(env1, env3, params, sell_m2=0, find_m3=0) {

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

  # Calculate the annual figures for the third mine, assuming it is found
  if(find_m3 %in% 1:4) {
    gold_prod_adj <- env3$gold_prod[1:(5-find_m3)]
    names(gold_prod_adj) <- names(env3$gold_prod)[(find_m3+1):5]
    m3_revenue <- gold_prod_adj * env3$gold_price[(find_m3+1):5]
    m3_costs <- (env3$selling_cost[(find_m3+1):5] * m3_revenue) + env3$equip_labour[(find_m3+1):5]
    m3_profit <- m3_revenue - m3_costs
    total_profit[,(find_m3+1):5] <- total_profit[,(find_m3+1):5] + m3_profit
  }

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
