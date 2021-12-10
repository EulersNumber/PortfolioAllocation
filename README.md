# PortfolioAllocation
This repository allows an investor to conduct Mean-Variance Optimization and Mean-CVar optimization for choosing optimal weights for financial assets in a portfolio. The investor can impose realistics constraints on the portfolio such as group- and box constraints on the individual asset weights and short selling constraints. In addition to the optimization, the user can view insightful plots of the efficient frontier and the optimal allocation, or view statistics and performance charts for different portfolios.

We have included example data on monthly returns from January 1994 to October 2021 for several asset classes in equities, fixed income, and REIT (proxy for real estate). See files monthly_ret_1994 in both .xlsx and .csv format for reference. However, the methods should work for any data of the same format if the user prefers to use her own data. 

Mean-Variance Optimization is implemented for both historical data and forward-looking estimates on returns, volatilities, correlations. While historical optimization requires only the time series of returns, forward-looking optimization requires the user to input expected returns and volatilities as a vector, and the correlation matrix can be perhaps more conveniently read from a .csv-file. See the proper format for the correlation matrix in the example data files (JPM_corr.csv, Invesco_corr.csv).

Mean-CVaR optimization is implemented only for historical data due to project scope. However, the NMOF-package that is used with forward-looking optimization also supports Mean-CVaR optimization. Hence, the user can extend the project by simulating a scenario matrix and using it for Mean-CVaR optimization if preferred.

## Sources

Historical optimization utilizes the [fPortfolio](https://cran.r-project.org/web/packages/fPortfolio/index.html)-package.

Forward-looking optimization utilizes the [NMOF](https://cran.r-project.org/web/packages/NMOF/index.html)-package.

## Contents

- Data
  - Test

