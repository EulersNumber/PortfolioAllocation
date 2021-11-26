# Library imports ----

library(tidyverse)
library(fPortfolio)
library(fAssets)
library(PerformanceAnalytics)

# Data specification ----

colnames(full_data)
pfolio <- full_data_ts[, c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap", "MSCI_EUR",
                           "LT_Treasury", "ST_Treasury", "HY_Corp_Bonds",
                           "REIT")]
c(start(pfolio), end(pfolio))
head(pfolio)
tail(pfolio)

pfolio_stats <- basicStats(pfolio)
pfolio_stats

# Long only ----

longOnlySpec <- portfolioSpec()
setRiskFreeRate(longOnlySpec) <- pfolio$ST_Treasury %>% mean()
setOptimize(longOnlySpec) <- "maxReturn"
longOnlySpec

pfolio_data <- portfolioData(pfolio, longOnlySpec)
pfolio_data
getNAssets(pfolio_data)
getMean(pfolio_data)
getSigma(pfolio_data) %>% diag()


portfolioConstraints(pfolio_data, longOnlySpec, combinedConstraints)

frontier <- portfolioFrontier(data = pfolio_data,
                              spec = longOnlySpec,
                              constraints = "LongOnly")
frontier
plot(frontier)


tailoredFrontierPlot(frontier, risk = "Sigma", sharpeRatio = F)

tangencyPfolio <- tangencyPortfolio(data = pfolio_data,
                                    spec = longOnlySpec,
                                    constraints = "LongOnly")
weightsPie(tangencyPfolio)

minVarPfolio <- minvariancePortfolio(data = pfolio_data,
                                     spec = longOnlySpec,
                                     constraints = "LongOnly")
weightsPie(minVarPfolio)

fAssets::assetsHistPlot(pfolio)
pfolioHist(pfolio, weights = tangencyPfolio@portfolio@portfolio$weights)

# Box and Group constraints ----

boxGroupSpec <- portfolioSpec()
setRiskFreeRate(boxGroupSpec) <- 0.1
setTargetReturn(boxGroupSpec) <- mean(pfolio)
boxGroupSpec

getMu(pfolio_data) %>% names()
boxConstraints <- c("minW[1:3]=0.05", "maxW[1:3]=0.3",
                    "minW[9] = 0.1", "maxW[9]=0.2")
groupConstraints <- c("minsumW[c(1:3)]=0.3", "maxsumW[c(1:3)]=0.5",
                      "minsumW[c(4,6,7,8)]=0.2", "maxsumW[c(4,6,7,8)]=0.3")
combinedConstraints <- c(boxConstraints, groupConstraints)

portfolioConstraints(pfolio_data, boxGroupSpec, combinedConstraints)

frontier <- portfolioFrontier(data = pfolio_data,
                              spec = boxGroupSpec,
                              constraints = combinedConstraints)
plot(frontier)

tailoredFrontierPlot
tailoredFrontierPlot(object = frontier,
                     return = "mean",
                     risk = "Sigma",
                     mText = "MV Portfolio - Box/Group Constraints")
tangencyPfolio <- tangencyPortfolio(data = pfolio_data,
                                    spec = boxGroupSpec,
                                    constraints = combinedConstraints)

minVarPfolio <- minvariancePortfolio(data = pfolio_data,
                                     spec = boxGroupSpec,
                                     constraints = combinedConstraints)

weightsPie(tangencyPfolio)
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


