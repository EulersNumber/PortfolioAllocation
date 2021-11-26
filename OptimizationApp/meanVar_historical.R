# Library imports ----

library(fPortfolio)
library(PerformanceAnalytics)

# Data specification ----

colnames(full_data)
pfolio <- full_data_ts[, c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap",
                           "LT_Treasury", "ST_Treasury", "HY_Corp_Bonds",
                           "REIT", "MSCI_EUR")]
c(start(pfolio), end(pfolio))
head(pfolio)
tail(pfolio)

pfolio_stats <- basicStats(pfolio)
pfolio_stats

# Optimization ----

pfolio_spec <- portfolioSpec()
setRiskFreeRate(pfolio_spec) <- 0.018/12 # US30T yield 
pfolio_spec

pfolio_data <- portfolioData(pfolio, pfolio_spec)
pfolio_data
getNAssets(pfolio_data)
getMean(pfolio_data)
getSigma(pfolio_data) %>% diag()

boxConstraints <- c("minW[1]=0.1", "maxW[1]=0.3",
                    "minW[2] = 0.05", "maxW[2]=0.2",
                    "minW[3] = 0.01", "maxW[3]=0.1",
                    "minW[4] = 0.1", "maxW[4]=0.2",
                    "minW[5] = 0.05", "maxW[5]=0.15",
                    "minW[6] = 0.01", "maxW[6]=0.1",
                    "minW[7] = 0.05", "maxW[7]=0.15",
                    "minW[8] = 0.05", "maxW[8]=0.2")
groupConstraints <- c("minsumW[c(1:3,8)]=0.45", "maxsumW[c(1:3,8)]=0.6",
                      "minsumW[c(4,5,6)]=0.25", "maxsumW[c(4,5,6)]=0.4")
combinedConstraints <- c(boxConstraints, groupConstraints)

portfolioConstraints(pfolio_data, pfolio_spec, combinedConstraints)

setNFrontierPoints(pfolio_spec) <- 50
frontier <- portfolioFrontier(data = pfolio_data,
                              spec = pfolio_spec,
                              constraints = combinedConstraints)
plot(frontier)
tailoredFrontierPlot(frontier, risk = "Sigma", sharpeRatio = F, title = F)
title(main = "Efficient Frontier", xlab = "Volatility", ylab = "Return")
graphics::legend(x= "topleft", legend=c("Minimum Variance Portfolio", "Tangency Portfolio"),
                col=c("red", "blue"), pch = 19, cex = .8, text.font=2)

tangencyPfolio <- tangencyPortfolio(data = pfolio_data,
                                    spec = longOnlySpec,
                                    constraints = combinedConstraints)
weightsPie(tangencyPfolio)
tangencyPfolio

minVarPfolio <- minvariancePortfolio(data = pfolio_data,
                                     spec = longOnlySpec,
                                     constraints = combinedConstraints)
weightsPie(minVarPfolio)

# Performance plotting ----

optWeights <- tangencyPfolio@portfolio@portfolio$weights
eqWeights <- rep(1/getNAssets(pfolio_data), getNAssets(pfolio_data))
minVarWeights <- minVarPfolio@portfolio@portfolio$weights

optPfolioRet <- Return.portfolio(pfolio, weights = optWeights)
eqWeightRet <- Return.portfolio(pfolio, weights = eqWeights)
minVarRet <- Return.portfolio(pfolio, weights = minVarWeights)

returns <- cbind(optPfolioRet, eqWeightRet)
colnames(returns) <- c("Tangency portfolio", "1/N")

chart.CumReturns(returns, lwd = 2, colorset=rich6equal, wealth.index = T,
                 legend.loc = "topleft", main = "Growth of 1$ invested")
charts.PerformanceSummary(returns, lwd = 2, wealth.index = F, colorset=rich6equal)
table.Stats(returns)
drawdownsStats(as.timeSeries(returns$`Tangency portfolio`))


