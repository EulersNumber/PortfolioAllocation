# PortfolioAllocation
This repository allows an investor to conduct Mean-Variance Optimization and Mean-CVar optimization for choosing optimal weights for financial assets in a portfolio.

We have included data on monthly returns from January 1994 to October 2021 for several asset classes in equities, fixed income, and REIT (proxy for real estate). However, the methods should work for any data of the same format.

Mean-Variance Optimization is possible for both historical data and forward-looking estimates on returns, volatilities, correlations. While historical optimization requires only the time series of returns, forward-looking optimization requires the user to input expected returns and volatilities as a vector, and the correlation matrix can be read from a .csv-file. See the proper format from the correlation data files (JPM_corr.csv, Invesco_corr.csv).

Mean-CVaR optimization is possible for only historical data due to project scope.
