# ============================================================
#  Monte Carlo Simulation — Triestine Briscola Casino Game
#  Deck: 40 cards, 4 suits x 10 ranks
#  Rules:
#    - Briscola suit picked uniformly at random each round
#    - Player starts with 120 points
#    - 10 cards dealt one at a time from shuffled deck
#    - Briscola card  → +10 points
#    - Non-briscola   → -20 points
#    - Payout multiplier = final_points / 120
#    - Net profit = (multiplier - 1) * bet
# ============================================================

set.seed(42)
N_SIMS    <- 100000
INIT_PTS  <- 120
N_CARDS   <- 10
SUITS     <- 4
CARDS_PER_SUIT <- 10   # 10 briscola cards out of 40

# ---- deck representation -----------------------------------
# 40 cards: suit 1..4, rank 1..10
deck_suits <- rep(1:SUITS, each = CARDS_PER_SUIT)

# ---- single round ------------------------------------------
play_round <- function(bet) {
  briscola_suit <- sample(1:SUITS, 1)          # random suit
  shuffled      <- sample(deck_suits)           # shuffle 40 cards
  hand          <- shuffled[1:N_CARDS]          # draw 10

  briscola_hits <- sum(hand == briscola_suit)
  non_briscola  <- N_CARDS - briscola_hits

  final_pts  <- INIT_PTS + briscola_hits * 10 - non_briscola * 20
  multiplier <- final_pts / INIT_PTS
  net_profit <- (multiplier - 1) * bet

  list(
    briscola_hits = briscola_hits,
    final_pts     = final_pts,
    multiplier    = multiplier,
    net_profit    = net_profit
  )
}

# ============================================================
#  STRATEGY COMPARISON
#  1. Flat bet       — always bet $10
#  2. Martingale     — double after every loss, reset on win
#  3. Anti-Martingale— double after every win, reset on loss
#  4. Kelly-inspired — bet proportional to remaining bankroll (5%)
# ============================================================

simulate_strategy <- function(strategy_name, n_sims = N_SIMS,
                               flat_bet = 10, start_bankroll = 1000,
                               kelly_frac = 0.05) {

  bankroll   <- numeric(n_sims + 1)
  net_profit <- numeric(n_sims)
  multipliers<- numeric(n_sims)
  hits_vec   <- numeric(n_sims)

  bankroll[1] <- start_bankroll
  current_bet <- flat_bet

  for (i in seq_len(n_sims)) {
    # Determine bet for this round
    bet <- switch(strategy_name,
      "Flat"           = flat_bet,
      "Martingale"     = current_bet,
      "Anti-Martingale"= current_bet,
      "Kelly"          = max(1, bankroll[i] * kelly_frac)
    )
    bet <- min(bet, bankroll[i])   # can't bet more than you have
    if (bet <= 0) { bet <- flat_bet; bankroll[i] <- start_bankroll }

    result <- play_round(bet)

    bankroll[i + 1]  <- bankroll[i] + result$net_profit
    net_profit[i]    <- result$net_profit
    multipliers[i]   <- result$multiplier
    hits_vec[i]      <- result$briscola_hits

    # Update bet for sequential strategies
    if (strategy_name == "Martingale") {
      current_bet <- if (result$net_profit < 0) min(current_bet * 2, bankroll[i+1]) else flat_bet
    } else if (strategy_name == "Anti-Martingale") {
      current_bet <- if (result$net_profit > 0) min(current_bet * 2, bankroll[i+1]) else flat_bet
    }
  }

  list(
    strategy    = strategy_name,
    bankroll    = bankroll,
    net_profit  = net_profit,
    multipliers = multipliers,
    hits        = hits_vec,
    final_bank  = bankroll[n_sims + 1],
    mean_profit = mean(net_profit),
    rtp         = mean(multipliers),
    house_edge  = 1 - mean(multipliers),
    win_rate    = mean(net_profit > 0),
    ruin_pct    = mean(bankroll[-1] <= 0)
  )
}

cat("\n===  Running simulations (100,000 rounds each)  ===\n\n")

strategies <- c("Flat", "Martingale", "Anti-Martingale", "Kelly")
results    <- lapply(strategies, simulate_strategy)
names(results) <- strategies

# ============================================================
#  ANALYTICAL BENCHMARKS
# ============================================================
# E[briscola hits] per 10 draws (hypergeometric: 10/40)
# E[hits] = 10 * (10/40) = 2.5
# E[final_pts] = 120 + 2.5*10 - 7.5*20 = 120 + 25 - 150 = -5
# E[multiplier] = -5/120 ≈ -0.0417  (player loses everything on average!)

expected_hits       <- N_CARDS * (CARDS_PER_SUIT / (SUITS * CARDS_PER_SUIT))
expected_final_pts  <- INIT_PTS + expected_hits * 10 - (N_CARDS - expected_hits) * 20
expected_multiplier <- expected_final_pts / INIT_PTS
theoretical_house_edge <- 1 - expected_multiplier

cat("─── Analytical (Exact) Benchmarks ─────────────────────\n")
cat(sprintf("  Expected briscola hits per round : %.4f / 10\n", expected_hits))
cat(sprintf("  Expected final points            : %.2f\n", expected_final_pts))
cat(sprintf("  Expected payout multiplier       : %.4f\n", expected_multiplier))
cat(sprintf("  Theoretical house edge           : %.2f%%\n\n", theoretical_house_edge * 100))

# ============================================================
#  RESULTS TABLE
# ============================================================
cat("─── Strategy Comparison ────────────────────────────────\n")
cat(sprintf("%-18s %10s %10s %10s %10s\n",
            "Strategy", "RTP", "House Edge", "Win Rate", "Ruin %"))
cat(strrep("─", 62), "\n")

for (r in results) {
  cat(sprintf("%-18s %9.2f%% %9.2f%% %9.2f%% %9.2f%%\n",
    r$strategy,
    r$rtp * 100,
    r$house_edge * 100,
    r$win_rate * 100,
    r$ruin_pct * 100
  ))
}

# ============================================================
#  PAYOUT DISTRIBUTION  (Flat strategy)
# ============================================================
flat  <- results[["Flat"]]
mults <- flat$multipliers

cat("\n─── Payout Multiplier Distribution (Flat Bet) ──────────\n")
cat(sprintf("  Min multiplier   : %.4f\n", min(mults)))
cat(sprintf("  Max multiplier   : %.4f\n", max(mults)))
cat(sprintf("  Mean multiplier  : %.4f\n", mean(mults)))
cat(sprintf("  Median multiplier: %.4f\n", median(mults)))
cat(sprintf("  Std dev          : %.4f\n", sd(mults)))
cat(sprintf("  P(multiplier > 1): %.2f%%\n", mean(mults > 1) * 100))
cat(sprintf("  P(multiplier = 0): %.2f%% (bust)\n", mean(mults <= 0) * 100))

# Briscola hits distribution
cat("\n─── Briscola Hits Distribution (out of 10 cards) ───────\n")
hits_tbl <- table(flat$hits)
for (h in names(hits_tbl)) {
  pct  <- hits_tbl[[h]] / N_SIMS * 100
  bar  <- strrep("█", round(pct / 1.5))
  pts  <- INIT_PTS + as.integer(h) * 10 - (N_CARDS - as.integer(h)) * 20
  mult <- pts / INIT_PTS
  cat(sprintf("  %2s hits → mult %+.3f | %5.2f%% %s\n", h, mult, pct, bar))
}

# ============================================================
#  SAVE PLOTS
# ============================================================
png("/mnt/user-data/outputs/briscola_simulation.png",
    width = 1400, height = 1100, res = 130)

par(mfrow = c(2, 2),
    bg     = "#0d1117",
    col.main = "#e6edf3",
    col.axis = "#8b949e",
    col.lab  = "#8b949e",
    fg       = "#30363d")

palette_cols <- c("#58a6ff", "#3fb950", "#f78166", "#d2a8ff")

# ── Plot 1: Payout multiplier histogram ─────────────────────
hist(mults,
     breaks  = 80,
     col     = "#1f3a5f",
     border  = "#58a6ff",
     main    = "Payout Multiplier Distribution",
     xlab    = "Multiplier (final_pts / 120)",
     ylab    = "Frequency",
     xlim    = c(min(mults) - 0.05, max(mults) + 0.05))
abline(v = 1,            col = "#3fb950", lwd = 2, lty = 2)
abline(v = mean(mults),  col = "#f78166", lwd = 2, lty = 1)
abline(v = expected_multiplier, col = "#d2a8ff", lwd = 2, lty = 3)
legend("topright",
       legend = c("Break-even (1.0)", "Simulated mean", "Theoretical mean"),
       col    = c("#3fb950", "#f78166", "#d2a8ff"),
       lwd    = 2, lty = c(2,1,3),
       bg     = "#161b22", text.col = "#e6edf3", cex = 0.8)

# ── Plot 2: Briscola hits bar chart ─────────────────────────
hits_freq <- table(factor(flat$hits, levels = 0:10)) / N_SIMS * 100
hit_cols  <- ifelse(
  INIT_PTS + as.integer(names(hits_freq)) * 10 -
    (N_CARDS - as.integer(names(hits_freq))) * 20 >= INIT_PTS,
  "#3fb950", "#f78166"
)
barplot(hits_freq,
        col    = hit_cols,
        border = NA,
        main   = "Briscola Hits per Round",
        xlab   = "Number of Briscola Cards in Hand",
        ylab   = "% of Rounds",
        names.arg = paste0(0:10))
legend("topright",
       legend = c("Profitable (mult ≥ 1)", "Loss (mult < 1)"),
       fill   = c("#3fb950", "#f78166"),
       bg     = "#161b22", text.col = "#e6edf3", cex = 0.8)

# ── Plot 3: Bankroll over time (first 2000 rounds, all strategies)
n_show <- 2000
plot(0:n_show, results[[1]]$bankroll[1:(n_show+1)],
     type = "n", ylim = c(0, 2000),
     main = "Bankroll Over First 2,000 Rounds",
     xlab = "Round", ylab = "Bankroll ($)")
abline(h = 1000, col = "#8b949e", lty = 2)
for (j in seq_along(strategies)) {
  lines(0:n_show, results[[strategies[j]]]$bankroll[1:(n_show+1)],
        col = palette_cols[j], lwd = 1.5)
}
legend("topleft",
       legend   = strategies,
       col      = palette_cols,
       lwd      = 2,
       bg       = "#161b22", text.col = "#e6edf3", cex = 0.8)

# ── Plot 4: Cumulative mean net profit per round (convergence)
plot(NULL, xlim = c(1, N_SIMS), ylim = c(-25, 5),
     main = "Convergence: Cumulative Mean Net Profit (Flat Bet)",
     xlab = "Number of Rounds Simulated",
     ylab = "Cumulative Mean Net Profit ($)")
abline(h = 0, col = "#8b949e", lty = 2)
cumulative_mean <- cumsum(flat$net_profit) / seq_len(N_SIMS)
lines(cumulative_mean, col = "#58a6ff", lwd = 1.5)
theoretical_mean <- (expected_multiplier - 1) * 10   # flat bet $10
abline(h = theoretical_mean, col = "#d2a8ff", lwd = 2, lty = 3)
legend("bottomright",
       legend = c("Simulated convergence", sprintf("Theoretical (%.2f$)", theoretical_mean)),
       col    = c("#58a6ff", "#d2a8ff"),
       lwd    = 2, lty = c(1, 3),
       bg     = "#161b22", text.col = "#e6edf3", cex = 0.8)

dev.off()
cat("\n✓ Plot saved → briscola_simulation.png\n")

# ============================================================
#  SAVE CSV SUMMARY
# ============================================================
summary_df <- data.frame(
  Strategy   = strategies,
  RTP_pct    = sapply(results, function(r) round(r$rtp * 100, 3)),
  HouseEdge_pct = sapply(results, function(r) round(r$house_edge * 100, 3)),
  WinRate_pct   = sapply(results, function(r) round(r$win_rate * 100, 3)),
  MeanNetProfit = sapply(results, function(r) round(r$mean_profit, 4)),
  RuinPct       = sapply(results, function(r) round(r$ruin_pct * 100, 3)),
  FinalBankroll = sapply(results, function(r) round(r$final_bank, 2))
)
write.csv(summary_df, "/mnt/user-data/outputs/briscola_summary.csv", row.names = FALSE)
cat("✓ Summary CSV saved → briscola_summary.csv\n\n")
