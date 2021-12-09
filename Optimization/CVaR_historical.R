# Library imports ----

library(fPortfolio)
library(tidyverse)
library(PerformanceAnalytics)
library(openxlsx)
library(RColorBrewer)
library(stringr)

# Data specification ----

full_data <- read.csv2("monthly_data_1994.csv") %>% as.tibble()
colnames(full_data)
full_data_assets <- full_data %>% select(!c(Y, M))
full_data_ts <- as.timeSeries(full_data_assets)
full_data_ts <- full_data_ts %>% 
  `setTime<-`(seq(as.Date("1994/1/1"), as.Date("2021/10/1"), by = "month"))
cvar.assets <- c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap", "LT_Treasury", "IMT_Treasury", "ST_Treasury", "HY_Corp_Bonds", "REIT")
cvar.portfolio <- full_data_ts[, cvar.assets]

str(cvar.portfolio)
basicStats(cvar.portfolio)
cor(cvar.portfolio)

# Portfolio specification ----

cvar.spec <- portfolioSpec()
setType(cvar.spec) <- "CVaR"
setSolver(cvar.spec) <- "solveRglpk.CVAR"
setAlpha(cvar.spec) <- 0.05
setNFrontierPoints(cvar.spec) <- 100
setRiskFreeRate(cvar.spec) <- 0.018/12 # US30T yield 
cvar.spec

cvar.portfolio.data <- portfolioData(cvar.portfolio)
cvar.portfolio.data

getNAssets(cvar.portfolio.data) # Number of assets
getMean(cvar.portfolio.data) # Mean
getSigma(cvar.portfolio.data) %>% diag() %>% sqrt() # StDev
(getMean(cvar.portfolio.data) - getRiskFreeRate(cvar.spec)) / (getSigma(cvar.portfolio.data) %>% diag() %>% sqrt()) # Sharpe

# Constraints ----

cvar.assets.str <- cvar.assets %>% gsub(pattern = "_", replacement = " ")
cvar.assets.str %>% as.data.frame() %>% `colnames<-`("Asset class") # indices for constraints
boxConstraints <- c("minW[1]=0.05", "maxW[1]=0.3",
                    "minW[2]=0.025","maxW[2]=0.25",
                    "minW[3]=0.01", "maxW[3]=0.2",
                    "maxW[4]=0.3",
                    "maxW[5]=0.3",
                    "maxW[6]=0.3",
                    "maxW[7]=0.1",
                    "minW[8]=0.025", "maxW[8]=0.2")
groupConstraints <- c("minsumW[c(1:3)]=0.4", "maxsumW[c(1:3)]=0.6",
                      "minsumW[c(4:7)]=0.25", "maxsumW[c(4:7)]=0.4")
combinedConstraints <- c(boxConstraints, groupConstraints)

# Optimization ----

cvar.frontier <- portfolioFrontier(cvar.portfolio, spec = cvar.spec, constraints = combinedConstraints)

# default efficient frontier plot
tailoredFrontierPlot(cvar.frontier,risk = "CVaR", sharpeRatio = F, title = F)
title(main = "CVaR Portfolio Frontier (Monthly returns 1994-2021)", xlab = "CVaR", ylab = "Return")
graphics::legend(x= "topleft", legend=c("Minimum CVaR Portfolio", "Tangency Portfolio"),
                 col=c("red", "blue"), pch = 19, cex = .8, text.font=2)

cvar.sortino.portfolio <- maxratioPortfolio(cvar.portfolio, cvar.spec, combinedConstraints)
cvar.minrisk.portfolio <- minriskPortfolio(cvar.portfolio, cvar.spec, combinedConstraints)

cvar.sortino.portfolio.ret <- getTargetReturn(cvar.sortino.portfolio)[2]
cvar.minrisk.portfolio.ret <- getTargetReturn(cvar.minrisk.portfolio)[2]

cvar.sortino.portfolio.cvar <- getTargetRisk(cvar.sortino.portfolio)[3]
cvar.minrisk.portfolio.cvar <- getTargetRisk(cvar.minrisk.portfolio)[3]

# export maximum Sortino allocation to Excel for plotting (optional)
cvar.sortino.portfolio.weights <- data.frame(cvar.assets.str, getWeights(cvar.sortino.portfolio))
colnames(cvar.sortino.portfolio.weights) <- c("Asset","Weight")
write.xlsx(cvar.sortino.portfolio.weights[order(cvar.sortino.portfolio.weights$Weight, decreasing = T),], "CVaR_hist_sortino_weights.xlsx", rownames = F)

# export minimum CVaR allocation to Excel for plotting (optional)
cvar.minrisk.portfolio.weights <- data.frame(cvar.assets.str, getWeights(cvar.minrisk.portfolio))
colnames(cvar.minrisk.portfolio.weights) <- c("Asset","Weight")
write.xlsx(cvar.minrisk.portfolio.weights[order(cvar.minrisk.portfolio.weights$Weight, decreasing = T),], "CVaR_hist_minrisk_weights.xlsx", rownames = F)

# Efficient frontier plotting ----

# graphical parameters must be adjusted for own needs (e.g., xlim and ylim)
plot(frontierPoints(cvar.frontier, frontier = "both"), ann = F, type = "l", pch = 20, panel.first = grid())
points(mv.minvar.portfolio.cvar, mv.minvar.portfolio.ret, col = "red", pch = 15)
points(mv.sharpe.portfolio.cvar, mv.sharpe.portfolio.ret, col = "green", pch = 15)
points(cvar.minrisk.portfolio.cvar, cvar.minrisk.portfolio.ret, col = "blue", pch = 17)
points(cvar.sortino.portfolio.cvar, cvar.sortino.portfolio.ret, col = "orange", pch = 17)
cvar.constraint <- 0.05525
abline(v = cvar.constraint, col = "red", lwd = 1, lty = 2)
title(main = "Mean-CVaR Frontier (monthly returns 1994-2021)", xlab = "CVaR", ylab = "Return")
legend(x= "topright", legend=c("Minimum Variance Portfolio","Maximum Sharpe Portfolio","Minimum CVaR Portfolio","Maximum Sortino Portfolio", "CVaR Constraint"), col=c( "red","green","blue","orange","red"), lty = c(NA, NA,NA,NA, 2), pch = c(15,15,17,17,NA), cex = .8, text.font=2)

# Allocation plotting ----

allocation_plotter(cvar.sortino.portfolio, box = F, radius = 0.6, cex = 0.8, col = c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B", "#231f8c", "#1d125c", "#1d125c"))
title(main = "Maximum Sortino Portfolio")

allocation_plotter(cvar.minrisk.portfolio, box = F, radius = 0.6, cex = 0.8, col = c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B", "#231f8c", "#1d125c", "#1d125c"))
title(main = "Minimum CVaR Portfolio")
