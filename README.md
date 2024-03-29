# PortfolioAllocation

This repository allows an investor to conduct Mean-Variance Optimization and Mean-CVaR optimization for choosing optimal weights for financial assets in a portfolio. The investor can impose realistics constraints on the portfolio such as group- and box constraints on the individual asset weights and short selling constraints. In addition to the optimization, the user can view insightful plots of the efficient frontier and the optimal allocation, or view statistics and performance charts for different portfolios.

We have included example data on monthly returns from January 1994 to October 2021 for several asset classes in equities, fixed income, and REIT (proxy for real estate). See files monthly_ret_1994 in both .xlsx and .csv format for reference. However, the methods should work for any data of the same format if the user prefers to use her own data. 

Mean-Variance Optimization is implemented for both historical data and forward-looking estimates on returns, volatilities, correlations. While historical optimization requires only the time series of returns, forward-looking optimization requires the user to input expected returns and volatilities as a vector, and the correlation matrix can be perhaps more conveniently read from a .csv-file. See the proper format for the correlation matrix in the example data files (JPM_corr.csv, Invesco_corr.csv).

Mean-CVaR optimization is implemented only for historical data due to project scope. However, the NMOF-package that is used with forward-looking optimization also supports Mean-CVaR optimization. Hence, the user can extend the project by simulating a scenario matrix and using it for Mean-CVaR optimization if preferred.

## Repository contents

Below is a list of the contents of the repository. The first level indicates the folder, and the second level indicates the file.

- Data
  - monthly_ret_1994.xlsx: example data on financial asset classes' monthly returns in .xlsx-format
  - monthly_ret_1994.csv: example data on financial asset classes' monthly returns in .csv-format
  - JPM_corr.csv: example data on a correlation matrix of financial asset classes in .csv-format. JP Morgan's estimates.
  - Invesco_corr.csv: example data on a correlation matrix of financial asset classes in .csv-format. Invesco's estimates.
- Optimization
  - helper_functions.R: a few helper functions to use along with optimization. Adviseable to run this file first
  - MV_historical.R: historical Mean-Variance optimization
  - CVaR_historical.R: historical Mean-CVaR optimization
  - MV_input_JPM.R: forward-looking optimization using JP Morgan's estimates on returns, volatilities, and correlations of several financial asset classes
  - MV_input_Invesco.R: forward-looking optimization using Invesco's estimates on returns, volatilities, and correlations of several financial asset classes
  - portfolio_metrics.R: statistics and performance plots for different portfolios. Adviseable to run this file last because it uses the optimal portfolios obtained from the historical optimization files by default

## Usability

In terms of usability, the optimal approach would have been to implement a portfolio optimizer app with a GUI, which would have allowed the user to optimize with different inputs without having to modify the code herself. However, this kind of an approach was unfortunately out of scope in our project. The current state of the project should be considered more of a backend implementation, or an optimization engine that lays out the necessry bulding blocks that an GUI (frontend) could then eventually use. Even though the current state of the tool allows tailored optimization for an user that is somewhat familiar with programming, building a smooth and well-functioning GUI is a natural next step and would essentially complete the tool. Stay tuned for the implementation!

## Sources

Market data:

- Thomson Reuters Eikon Datastream
- [Yahoo! Finance](https://finance.yahoo.com/)

Optimization:

- Historical optimization utilizes the [fPortfolio](https://cran.r-project.org/web/packages/fPortfolio/index.html)-package.
- Forward-looking optimization utilizes the [NMOF](https://cran.r-project.org/web/packages/NMOF/index.html)-package.

