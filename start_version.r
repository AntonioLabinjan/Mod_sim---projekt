# -------------------------------------------------------------------------
# PROJECT: Monte Carlo Simulation of the Martingale Betting System
# COURSE: Computational Statistics / Probability Theory
# -------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# --- GLOBAL PARAMETERS ---
SET_SEED      <- 42
SIMULATIONS   <- 10000     # Total number of "parallel universes"
START_BANK    <- 1000      # Starting capital
TARGET_BANK   <- 2000      # Goal (Double or nothing)
BASE_BET      <- 10        # Minimum bet
TABLE_LIMIT   <- 500       # Maximum allowed bet by the casino
WIN_CHANCE    <- 18/37     # European Roulette (0.4865)

set.seed(SET_SEED)

# --- CORE SIMULATION ENGINE ---
simulate_gambler <- function(id, start, target, base_bet, limit, prob) {
  capital <- start
  current_bet <- base_bet
  history <- c(capital)
  
  while (capital > 0 && capital < target) {
    if (runif(1) < prob) {
      # WIN: Add profit and reset bet
      capital <- capital + current_bet
      current_bet <- base_bet
    } else {
      # LOSS: Subtract stake and double bet (Martingale)
      capital <- capital - current_bet
      current_bet <- current_bet * 2
    }
    
    # Apply Table Limit and Bankroll Constraints
    if (current_bet > limit) current_bet <- limit
    if (current_bet > capital) current_bet <- capital
    
    history <- c(history, capital)
    
    # Safety break to prevent infinite loops in fair-game simulations
    if (length(history) > 10000) break
  }
  
  return(list(
    data = data.frame(Hand = 0:(length(history)-1), Capital = history, ID = id),
    final = tail(history, 1)
  ))
}

# --- EXECUTION: MASSIVE PARALLEL SIMULATION ---
cat("Running", SIMULATIONS, "simulations...\n")

results_list <- lapply(1:SIMULATIONS, function(i) {
  # We only save full paths for the first 50 for visualization (memory efficiency)
  if (i <= 50) {
    return(simulate_gambler(i, START_BANK, TARGET_BANK, BASE_BET, TABLE_LIMIT, WIN_CHANCE))
  } else {
    # For the rest, just return the final result
    return(list(final = simulate_gambler(i, START_BANK, TARGET_BANK, BASE_BET, TABLE_LIMIT, WIN_CHANCE)$final))
  }
})

# --- DATA AGGREGATION ---
final_balances <- sapply(results_list, function(x) x$final)
paths_df <- bind_rows(lapply(results_list[1:50], function(x) x$data))

stats <- data.frame(
  Outcome = ifelse(final_balances >= TARGET_BANK, "Success", "Bankruptcy"),
  Value = final_balances
)

# --- PRINT ACADEMIC SUMMARY ---
summary_table <- stats %>%
  group_by(Outcome) %>%
  summarise(Count = n(), Percentage = (n() / SIMULATIONS) * 100)

print(summary_table)
cat("Expected Value (EV) of the strategy:", mean(final_balances), "€\n")

# --- VISUALIZATION ---

# Plot 1: The "Death Spiral" - Trajectories of 50 Gamblers
p1 <- ggplot(paths_df, aes(x = Hand, y = Capital, group = ID, color = Capital)) +
  geom_line(alpha = 0.6) +
  scale_color_gradientn(colors = c("#e74c3c", "#f1c40f", "#2ecc71")) +
  geom_hline(yintercept = TARGET_BANK, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "darkred", size = 1) +
  labs(title = "Stochastic Trajectories of Martingale Players",
       subtitle = "Visualizing the extreme volatility and sudden bankruptcy",
       x = "Number of Hands Played", y = "Bankroll (€)") +
  theme_minimal()

# Plot 2: Final Wealth Distribution (The Binary Reality)
p2 <- ggplot(stats, aes(x = Outcome, fill = Outcome)) +
  geom_bar() +
  scale_fill_manual(values = c("Bankruptcy" = "#c0392b", "Success" = "#27ae60")) +
  labs(title = "Final Outcomes Distribution",
       subtitle = paste("Based on", SIMULATIONS, "Monte Carlo iterations"),
       x = "Final State", y = "Frequency") +
  theme_minimal()

print(p1)
print(p2)
