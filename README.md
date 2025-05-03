# Market Risk Engine – VaR & Expected Shortfall

This project implements three methods of Value-at-Risk (VaR) and Expected Shortfall (ES) calculation — Historical, Parametric (Variance-Covariance), and Monte Carlo — on a diversified portfolio of equities and bonds. It supports stress testing and performance validation using statistical backtesting.

## Features
- Monte Carlo Simulation and Parametric Methods for Value at Risk (VaR)
- Expected Shortfall (ES) and Volatility Computation
- Correlation Matrix and Stress Testing
- Credit Risk Quantification (PD, LGD, EAD modelling)
- Interactive Dashboard using R Shiny

## Files
- `risk_model_report.Rmd` - Main report integrating modelling and results
- `risk_dashboard_app.R` - Interactive dashboard for simulation and visualization


## Technologies Used
- R (Shiny, Plotly, Quantmod, PerformanceAnalytics)
- R Markdown for documentation

## Getting Started
1. Clone the repository
2. Open `risk_model_report.Rmd` in RStudio
3. Run the Shiny app (`risk_dashboard_app.R`) to interact with simulations


## Requirements
- R (version 4.0+ recommended)
- R Packages: shiny, plotly, quantmod, PerformanceAnalytics, MASS, corrplot, caret, randomForest, rmarkdown

## 👤 Author

Amanda Achiangia  
BSc Applied Mathematics (Financial Mathematics), York University  
Aspiring Quantitative Finance Professional  
[LinkedIn](https://www.linkedin.com/in/amanda-a-602328284/) | [GitHub](https://github.com/sensor-aae)
