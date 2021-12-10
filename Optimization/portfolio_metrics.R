# Notes ----

# One must optimize first, i.e., run the codes in MV_historical.R and CVaR_historical.R first
# That way, one obtains the tangency- and min risk portfolios for both methods
# Those portfolios are then used here along with the EWP that is formed in this file

# Data manipulation ----

# EWP specification
mv.eq.spec <- portfolioSpec()
setRiskFreeRate(mv.eq.spec) <- 0.018/12 # US30T yield
setNFrontierPoints(mv.eq.spec) <- 100
setWeights(mv.eq.spec) <- rep(1/getNAssets(mv.portfolio.data), getNAssets(mv.portfolio.data))

# EWP metrics
mv.eq.portfolio <- feasiblePortfolio(data = mv.portfolio, spec = mv.eq.spec, constraints = combinedConstraints)
mv.eq.portfolio.ret <- getTargetReturn(mv.eq.portfolio)[2]
mv.eq.portfolio.sigma <- getTargetRisk(mv.eq.portfolio)[2]

# Portfolio weights
mv.eq.weights <- getWeights(mv.eq.portfolio)
mv.sharpe.weights <- getWeights(mv.sharpe.portfolio)
cvar.sortino.weights <- getWeights(cvar.sortino.portfolio)
mv.minvar.weights <- getWeights(mv.minvar.portfolio)
cvar.minrisk.weights <- getWeights(cvar.minrisk.portfolio)

# Portfolio returns
mv.eq.ret <- Return.portfolio(mv.portfolio, weights = mv.eq.weights)
mv.sharpe.ret <- Return.portfolio(mv.portfolio, weights = mv.sharpe.weights)
cvar.sortino.ret <- Return.portfolio(cvar.portfolio, weights = cvar.sortino.weights)
mv.minvar.ret <- Return.portfolio(mv.portfolio, weights = mv.minvar.weights)
cvar.minrisk.ret <- Return.portfolio(cvar.portfolio, weights = cvar.minrisk.weights)
returns <- cbind(mv.eq.ret, mv.sharpe.ret, cvar.sortino.ret, mv.minvar.ret, cvar.minrisk.ret)
colnames(returns) <- c("EWP", "Max Sharpe", "Max Sortino", "Min Var", "Min CVaR")

# Statistics ----

# standard statistics
stats <- basicStats(returns) %>% as.data.frame()
stats <- rownames_to_column(stats)
# Export return stats to Excel (optional)
write.xlsx(stats, "return_stats.xlsx")

# Drawdown statistics
apply(returns, 2 , function(data) drawdownsStats(as.timeSeries(data)))

# Performance plot ----

?chart.TimeSeries.base
chart.CumReturns(returns, lwd = 1, wealth.index = T,
                 legend.loc = "topleft", main = "Growth of 1$ invested",
                 cex.legend = 1.2)
