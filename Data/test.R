library(quantmod)

getSymbols("AAPL", from = "2022-01-01", to = "2023-01-01")
returns <- dailyReturn(Cl(AAPL))
write.csv(returns, "AAPL_returns.csv", row.names = FALSE)


