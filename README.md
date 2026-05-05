# Momentum Rotation Strategy Backtest (2016–2025)

A quarterly momentum rotation strategy implemented in R, backtested on three U.S. stocks—Microsoft (MSFT), Morgan Stanley (MS), and ADMA Biologics (ADMA)—and benchmarked against the S&P 500. The strategy ranks stocks by their average daily returns over the last three months, tilts weights toward the top performers, rebalances quarterly, and includes realistic transaction costs.

## Strategy Rules
- **Universe**: ADMA, MSFT, MS  
- **Signal**: Average daily percentage return over the previous 3 calendar months  
- **Ranking**: Stocks ranked 1–3 each month; quarterly ranking based on average of the three monthly ranks  
- **Rebalancing**: First trading day of January, April, July, October  
- **Weights**: Top seed 37.5%, Middle seed 37.5%, Bottom seed 25%  
- **Transaction costs**: 0.5% of turnover per rebalance  
- **Initial capital**: $1,000,000  
- **Long-only, no leverage**

## Key Results (2016–2025)

| Metric                       | Portfolio   | S&P 500     |
|------------------------------|-------------|-------------|
| Total Return                 | +766.2%     | +242.6%     |
| Annualised Return            | 24.2%       | 13.1%       |
| Max Drawdown                 | -40.6%      | -33.9%      |
| Sharpe Ratio (annualised)    | 0.86        | 0.77        |
| Number of Rebalances         | 29          | –           |

> The portfolio significantly outperformed the benchmark, but with deeper drawdowns and higher concentration risk—especially from Microsoft’s AI-driven rally post-2022.

## Visuals

<img width="1268" height="1030" alt="image" src="https://github.com/user-attachments/assets/dc48009c-7d9c-44c8-ba7b-1c1127303f78" />
  
<img width="1268" height="1030" alt="image" src="https://github.com/user-attachments/assets/e5e55325-fbf2-43af-b690-55da11e278fb" />

## Code and Reproducibility

### Requirements
- R (≥ 4.0 recommended)
- R packages: `quantmod`, `ggplot2`, `xts`, `knitr` (for tables)

### How to Run
1. Clone this repo:
   ```bash
   git clone (https://github.com/unluckycraig37/Momentum-Backtesting-Strategy)
