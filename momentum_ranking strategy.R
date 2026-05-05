# ==============================================================================
# AMA3232 Programming for Data Science - Mini-Project
# Momentum Rotation Strategy (Quarterly Rebalancing)
# Stocks: ADMA (Biotech), MSFT (Technology), MS (Financials)
# Period: 1 Jan 2016 – 31 Dec 2025
# Initial Capital: $1,000,000 USD
# Trading Cost: 0.5% per rebalance
# ==============================================================================

# --------------------- 1. CLEANING & DATA IMPORT -----------------------------
library(quantmod)
library(ggplot2)
# Download adjusted close prices for our three stocks + benchmark
# Using Yahoo Finance via quantmod (automatically handles adjusted closes)
ADMA <- getSymbols("ADMA", src = 'yahoo', 
                   from = "2016-01-01", to = "2025-12-31", 
                   auto.assign = FALSE)

MSFT <- getSymbols("MSFT", src = 'yahoo', 
                   from = "2016-01-01", to = "2025-12-31", 
                   auto.assign = FALSE)

MS <- getSymbols("MS", src = 'yahoo', 
                 from = "2016-01-01", to = "2025-12-31", 
                 auto.assign = FALSE)

# Benchmark: S&P 500
sp500 <- getSymbols("^GSPC", src = 'yahoo', 
                    from = "2016-01-01", to = "2025-12-31", 
                    auto.assign = FALSE)

# Quick check for missing values (should be zero after clean data)
sum(is.na(ADMA))
sum(is.na(MSFT))
sum(is.na(MS))
sum(is.na(sp500))
# Verify that all four data series have identical number of rows (i.e., same trading days)
cat("\n=== Date Alignment Check ===\n")
cat("ADMA rows:", nrow(ADMA), "| MSFT rows:", nrow(MSFT), 
    "| MS rows:", nrow(MS), "| S&P 500 rows:", nrow(sp500), "\n")
if (nrow(ADMA) == nrow(MSFT) && nrow(MSFT) == nrow(MS) && nrow(MS) == nrow(sp500)) {
  cat("All series contain the same number of trading days. No missing dates require handling.\n")
} else {
  cat("Warning: Row counts differ. Data alignment would be needed.\n")
}

# --------------------- 2. INITIAL PORTFOLIO SETUP -----------------------------
Capital <- 1000000          # Starting capital = $1,000,000 USD

# Initial equal-weight allocation (≈33.33% each) on Day 1
shares_bought <- c((Capital/3)/ADMA$ADMA.Close[[1]],
                   (Capital/3)/MSFT$MSFT.Close[[1]],
                   (Capital/3)/MS$MS.Close[[1]])

weights        <- c(rep(1/3, 3))      # Current weights (will be updated quarterly)
target_weights <- c(0.375, 0.375, 0.25)  # Target weights after ranking: 37.5/37.5/25

trades           <- 0                 # Counter for number of rebalances
portfolio_value  <- Capital           # Current portfolio value
daily_portfolio  <- c()               # Vector to store daily portfolio values

# --------------------- 3. MOMENTUM CALCULATION (Main Backtest Loop) -----------
# This is the core of the strategy: quarterly momentum ranking + rebalancing

last_row <- nrow(ADMA)      # Total number of trading days
current_row <- 1            # Pointer to the current trading day we are processing

while (current_row <= last_row) {
  
  # Monthly Ranking (we process 3 months at a time = 1 quarter)
  n_quarters <- 0
  monthly_rankings <- list(month1 = c(), month2 = c(), month3 = c())
  
  for (quarter in 1:3) {
    
    if (current_row > last_row) break
    
    # ------------------- Find the last trading day of this month -------------
    month_start   <- current_row
    iterative_day <- current_row
    month         <- as.integer(substring(as.character(index(ADMA)[iterative_day]), 6, 7))
    
    month0 <- month
    x      <- iterative_day
    while (x < last_row) {
      next_day_month <- as.integer(substring(as.character(index(ADMA)[x + 1]), 6, 7))
      if (next_day_month != month0) break
      x <- x + 1
    }
    month_end <- x                     # Last trading day of the current month
    
    # Safety check for very short/partial months at the end of data
    if (month_end < month_start) {
      current_row <- last_row + 1
      break
    }
    
    # ------------------- Record portfolio value on first day of month --------
    A_day0  <- ADMA$ADMA.Close[[month_start]]
    Mi_day0 <- MSFT$MSFT.Close[[month_start]]
    Ms_day0 <- MS$MS.Close[[month_start]]
    
    portfolio_value <- A_day0 * shares_bought[1] +
      Mi_day0 * shares_bought[2] +
      Ms_day0 * shares_bought[3]
    daily_portfolio <- c(daily_portfolio, portfolio_value)
    
    # ------------------- Calculate daily momentum for the month --------------
    day_change <- list(ADMA = c(), MSFT = c(), MS = c())
    
    for (i in (month_start + 1):month_end) {
      A_day  <- ADMA$ADMA.Close[[i]]
      Mi_day <- MSFT$MSFT.Close[[i]]
      Ms_day <- MS$MS.Close[[i]]
      
      # Daily percentage return (no look-ahead bias)
      A_pct  <- (A_day  / A_day0 - 1) * 100
      Mi_pct <- (Mi_day / Mi_day0 - 1) * 100
      Ms_pct <- (Ms_day / Ms_day0 - 1) * 100
      
      day_change$ADMA <- c(day_change$ADMA, A_pct)
      day_change$MSFT <- c(day_change$MSFT, Mi_pct)
      day_change$MS   <- c(day_change$MS,   Ms_pct)
      
      # Update portfolio value for this day
      portfolio_value <- A_day * shares_bought[1] + 
        Mi_day * shares_bought[2] + 
        Ms_day * shares_bought[3]
      daily_portfolio <- c(daily_portfolio, portfolio_value)
      
      # Move forward one day
      A_day0  <- A_day
      Mi_day0 <- Mi_day
      Ms_day0 <- Ms_day
    }
    
    # ------------------- Rank the stocks based on average daily return -------
    average_day_change <- c(mean(day_change$ADMA), 
                            mean(day_change$MSFT), 
                            mean(day_change$MS))
    
    seed1 <- which.max(average_day_change)   # Best performer
    seed3 <- which.min(average_day_change)   # Worst performer
    seed2 <- 6 - seed1 - seed3               # Middle performer
    
    # Store ranking for this month (used for voting later)
    if (seed1 == 1 & seed2 == 2 & seed3 == 3) {
      monthly_rankings[[quarter]] <- c("ADMA", "MSFT", "MS")
    } else if (seed1 == 1 & seed2 == 3 & seed3 == 2) {
      monthly_rankings[[quarter]] <- c("ADMA", "MS", "MSFT")
    } else if (seed1 == 2 & seed2 == 1 & seed3 == 3) {
      monthly_rankings[[quarter]] <- c("MSFT", "ADMA", "MS")
    } else if (seed1 == 2 & seed2 == 3 & seed3 == 1) {
      monthly_rankings[[quarter]] <- c("MSFT", "MS", "ADMA")
    } else if (seed1 == 3 & seed2 == 2 & seed3 == 1) {
      monthly_rankings[[quarter]] <- c("MS", "MSFT", "ADMA")
    } else if (seed1 == 3 & seed2 == 1 & seed3 == 2) {
      monthly_rankings[[quarter]] <- c("MS", "ADMA", "MSFT")
    }
    
    # Move to the next month
    current_row <- month_end + 1
    n_quarters  <- n_quarters + 1
  }
  
  # --------------------- 4. REBALANCING (only after full 3-month quarter) -----
  if (n_quarters == 3) {
    
    # Voting: count how many times each stock ranked 1st, 2nd, or 3rd
    A_ranks  <- c()
    Mi_ranks <- c()
    Ms_ranks <- c()
    
    for (i in 1:3) {
      if (monthly_rankings[[i]][1] == "ADMA") A_ranks  <- c(A_ranks,  1)
      if (monthly_rankings[[i]][1] == "MSFT") Mi_ranks <- c(Mi_ranks, 1)
      if (monthly_rankings[[i]][1] == "MS")   Ms_ranks <- c(Ms_ranks, 1)
      
      if (monthly_rankings[[i]][2] == "ADMA") A_ranks  <- c(A_ranks,  2)
      if (monthly_rankings[[i]][2] == "MSFT") Mi_ranks <- c(Mi_ranks, 2)
      if (monthly_rankings[[i]][2] == "MS")   Ms_ranks <- c(Ms_ranks, 2)
      
      if (monthly_rankings[[i]][3] == "ADMA") A_ranks  <- c(A_ranks,  3)
      if (monthly_rankings[[i]][3] == "MSFT") Mi_ranks <- c(Mi_ranks, 3)
      if (monthly_rankings[[i]][3] == "MS")   Ms_ranks <- c(Ms_ranks, 3)
    }
    
    mean_A_ranks  <- mean(A_ranks)
    mean_Mi_ranks <- mean(Mi_ranks)
    mean_Ms_ranks <- mean(Ms_ranks)
    
    # Tie-breaker logic (use the most recent ranking)
    if (mean_A_ranks == mean_Mi_ranks & mean_A_ranks == mean_Ms_ranks & mean_Ms_ranks == mean_Mi_ranks) {
      mean_A_ranks  <- A_ranks[3]
      mean_Mi_ranks <- Mi_ranks[3]
      mean_Ms_ranks <- Ms_ranks[3]
    } else if (mean_A_ranks == mean_Ms_ranks) {
      mean_A_ranks  <- A_ranks[3]
      mean_Ms_ranks <- Ms_ranks[3]
    } else if (mean_A_ranks == mean_Mi_ranks) {
      mean_A_ranks  <- A_ranks[3]
      mean_Mi_ranks <- Mi_ranks[3]
    } else if (mean_Ms_ranks == mean_Mi_ranks) {
      mean_Ms_ranks <- Ms_ranks[3]
      mean_Mi_ranks <- Mi_ranks[3]
    }
    
    ranks <- c(mean_A_ranks, mean_Mi_ranks, mean_Ms_ranks)
    
    topseed    <- which.min(ranks)      # Best overall
    bottomseed <- which.max(ranks)      # Worst overall
    middle     <- 6 - topseed - bottomseed
    
    # Apply new target weights
    target_weights[topseed]    <- 0.375
    target_weights[middle]     <- 0.375
    target_weights[bottomseed] <- 0.25
    
    # Calculate turnover and transaction costs (0.5%)
    turnover        <- 0.5 * sum(abs(target_weights - weights))
    turnover_dollar <- turnover * portfolio_value
    transaction_cost <- 0.005 * turnover_dollar
    
    portfolio_value <- portfolio_value - transaction_cost   # Deduct cost
    
    daily_portfolio[length(daily_portfolio)] <- portfolio_value
    
    if (turnover > 0) trades <- trades + 1                 # Count the trade
    
    weights <- target_weights                               # Update current weights
    
    # Rebalance shares on the last day of the quarter
    rebalance_day <- month_end
    
    stock <- function(n) {
      if (n == 1) return(ADMA$ADMA.Close[[rebalance_day]])
      else if (n == 2) return(MSFT$MSFT.Close[[rebalance_day]])
      return(MS$MS.Close[[rebalance_day]])
    }
    
    shares_bought[topseed]    <- (0.375 * portfolio_value) / stock(topseed)
    shares_bought[middle]     <- (0.375 * portfolio_value) / stock(middle)
    shares_bought[bottomseed] <- (0.25  * portfolio_value) / stock(bottomseed)
  }
}

# --------------------- 5. CONVERT TO xts OBJECT FOR ANALYSIS -----------------
daily_portfolio <- xts(daily_portfolio, 
                       order.by = index(ADMA)[1:length(daily_portfolio)])

# --------------------- 6. FINAL RESULTS & VISUALS ---------------------

# Create S&P 500 equity curve (same starting capital)
sp500_equity <- Capital * (sp500$GSPC.Close / as.numeric(sp500$GSPC.Close[1]))

# === Calculate Drawdown ===
portfolio_dd <- (daily_portfolio / cummax(daily_portfolio) - 1) * 100
sp500_dd     <- (sp500_equity     / cummax(sp500_equity)     - 1) * 100

# Prepare data for ggplot2
plot_data <- data.frame(
  Date      = index(daily_portfolio),
  Portfolio = as.numeric(daily_portfolio),
  SP500     = as.numeric(sp500_equity)
)

drawdown_data <- data.frame(
  Date      = index(daily_portfolio),
  Portfolio = as.numeric(portfolio_dd),
  SP500     = as.numeric(sp500_dd)
)

# === 1. Equity Curve Plot ===
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Portfolio, color = "Momentum Portfolio"), linewidth = 1.2) +
  geom_line(aes(y = SP500, color = "S&P 500 Benchmark"), linewidth = 1.0) +
  labs(title = "Portfolio Equity Curve vs S&P 500 Benchmark",
       y = "Portfolio Value ($)",
       x = "Date") +
  scale_color_manual(values = c("Momentum Portfolio" = "blue", 
                                "S&P 500 Benchmark" = "red")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# === 2. Drawdown Comparison Plot ===
ggplot(drawdown_data, aes(x = Date)) +
  geom_line(aes(y = Portfolio, color = "Momentum Portfolio"), linewidth = 1.1) +
  geom_line(aes(y = SP500, color = "S&P 500 Benchmark"), linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Drawdown Comparison: Portfolio vs S&P 500",
       y = "Drawdown (%)",          # Negative = loss
       x = "Date") +
  scale_color_manual(values = c("Momentum Portfolio" = "blue", 
                                "S&P 500 Benchmark" = "red")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# --------------------- 7. PERFORMANCE SUMMARY ---------------------
portfolio_returns <- dailyReturn(daily_portfolio)
sharpe_ratio      <- mean(portfolio_returns) / sd(portfolio_returns) * sqrt(252)

sp500_returns <- dailyReturn(sp500$GSPC.Close)
sp500_sharpe  <- mean(sp500_returns) / sd(sp500_returns) * sqrt(252)

# Max Drawdown
portfolio_max_dd <- min(portfolio_dd)
sp500_max_dd     <- min(sp500_dd)

total_return      <- as.numeric(tail(daily_portfolio, 1)) / Capital - 1
days              <- length(daily_portfolio)
years             <- days / 252
annualized_return <- (1 + total_return)^(1/years) - 1

cat("\n=== FINAL BACKTEST RESULTS ===\n")
cat("Total Return:                ", round(total_return * 100, 2), "%\n")
cat("Annualized Return:           ", round(annualized_return * 100, 2), "%\n")
cat("Max Drawdown (Portfolio):    ", round(portfolio_max_dd, 2), "%\n")
cat("Max Drawdown (S&P 500):      ", round(sp500_max_dd, 2), "%\n")
cat("Number of Trades:            ", trades, "\n")
cat("Sharpe Ratio (Portfolio):    ", round(sharpe_ratio, 3), "\n")
cat("Sharpe Ratio (S&P 500):      ", round(sp500_sharpe, 3), "\n")
cat("vs S&P 500 Total Return:     ", 
    round((as.numeric(tail(sp500$GSPC.Close,1)) / as.numeric(sp500$GSPC.Close[1]) - 1) * 100, 2), "%\n")
#--------------------- 8. SUMMARY STATISTICS FOR THE THREE STOCKS ------------
  stock_returns <- merge(
    dailyReturn(ADMA$ADMA.Close, type = "log"),
    dailyReturn(MSFT$MSFT.Close, type = "log"),
    dailyReturn(MS$MS.Close, type = "log")
  )
colnames(stock_returns) <- c("ADMA", "MSFT", "MS")

annual_return <- colMeans(stock_returns, na.rm = TRUE) * 252
annual_vol    <- apply(stock_returns, 2, sd, na.rm = TRUE) * sqrt(252)
corr_matrix   <- cor(stock_returns, use = "complete.obs")

summary_table <- data.frame(
  Stock = c("ADMA", "MSFT", "MS"),
  `Annualized Return (%)`     = round(annual_return * 100, 2),
  `Annualized Volatility (%)` = round(annual_vol * 100, 2)
)

cat("\n=== SUMMARY STATISTICS TABLE (2016–2025) ===\n")
print(summary_table, row.names = FALSE)

cat("\n=== CORRELATION MATRIX (Daily Log Returns) ===\n")
print(round(corr_matrix, 3))