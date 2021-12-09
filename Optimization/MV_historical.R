# Library imports ----

library(fPortfolio)
library(tidyverse)
library(PerformanceAnalytics)
library(openxlsx)
library(RColorBrewer)
library(stringr)

# Data specification ----

full_data <- read.csv2("monthly_data_1994.csv", header = T) %>% as.tibble()
colnames(full_data)
full_data_assets <- full_data %>% select(!c(Y, M))
full_data_ts <- as.timeSeries(full_data_assets)
full_data_ts <- full_data_ts %>% 
  `setTime<-`(seq(as.Date("1994/1/1"), as.Date("2021/10/1"), by = "month"))
mv.assets <- c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap", "LT_Treasury", "IMT_Treasury", "ST_Treasury", "HY_Corp_Bonds", "REIT")
mv.portfolio <- full_data_ts[, mv.assets]

str(mv.portfolio)
basicStats(mv.portfolio)
cor(mv.portfolio)

# Portfolio specification ----

mv.spec <- portfolioSpec()
setType(mv.spec) <- "MV"
setSolver(mv.spec) <- "solveRquadprog"
setRiskFreeRate(mv.spec) <- 0.018/12 # US30T yield 
setNFrontierPoints(mv.spec) <- 100
mv.spec

mv.portfolio.data <- portfolioData(mv.portfolio, mv.spec)
mv.portfolio.data

getNAssets(mv.portfolio.data) # Number of assets
getMean(mv.portfolio.data) # Mean
getSigma(mv.portfolio.data) %>% diag() %>% sqrt() # StDev
(getMean(mv.portfolio.data) - getRiskFreeRate(mv.spec)) / getSigma(mv.portfolio.data) %>% diag() %>% sqrt() # Sharpe

# Constraints ----

mv.assets.str <- mv.assets %>% gsub(pattern = "_", replacement = " ")
mv.assets.str %>% as.data.frame() %>% `colnames<-`("Asset class") # indices for constraints
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

mv.frontier <- portfolioFrontier(data = mv.portfolio, spec = mv.spec, constraints = combinedConstraints)

# default efficient frontier plot
tailoredFrontierPlot(mv.frontier, sharpeRatio = F, title = F)
title(main = "MV Portfolio Frontier (Monthly returns 1994-2021)", xlab = "Volatility", ylab = "Return")
graphics::legend(x= "topleft", legend=c("Minimum CVaR Portfolio", "Tangency Portfolio"),
                 col=c("red", "blue"), pch = 19, cex = .8, text.font=2)

mv.sharpe.portfolio <- tangencyPortfolio(data = mv.portfolio, spec = mv.spec, constraints = combinedConstraints)
mv.minvar.portfolio <- minvariancePortfolio(data = mv.portfolio, spec = mv.spec, constraints = combinedConstraints)

mv.sharpe.portfolio.ret <- getTargetReturn(mv.sharpe.portfolio)[2]
mv.minvar.portfolio.ret <- getTargetReturn(mv.minvar.portfolio)[2]
  
mv.sharpe.portfolio.sigma <- getTargetRisk(mv.sharpe.portfolio)[2]
mv.minvar.portfolio.sigma <- getTargetRisk(mv.minvar.portfolio)[2]

mv.sharpe.portfolio.cvar <- getTargetRisk(mv.sharpe.portfolio)[3]
mv.minvar.portfolio.cvar <- getTargetRisk(mv.minvar.portfolio)[3]

# export maximum Sharpe allocation to Excel for plotting (optional)
mv.sharpe.portfolio.weights <- data.frame(mv.assets.str, getWeights(mv.sharpe.portfolio))
colnames(mv.sharpe.portfolio.weights) <- c("Asset","Weight")
write.xlsx(mv.sharpe.portfolio.weights[order(mv.sharpe.portfolio.weights$Weight, decreasing = T),], "MV_hist_sharpe_weights.xlsx", rownames = F)

# export Minimum Variance allocation to Excel for plotting (optional)
mv.minvar.portfolio.weights <- data.frame(mv.assets.str, getWeights(mv.minvar.portfolio))
colnames(mv.minvar.portfolio.weights) <- c("Asset","Weight")
write.xlsx(mv.minvar.portfolio.weights[order(mv.minvar.portfolio.weights$Weight, decreasing = T),], "MV_hist_minvar_weights.xlsx", rownames = F)

# Efficient frontier plotting ----

# graphical parameters must be adjusted for own needs (e.g., xlim and ylim)
plot(frontierPoints(mv.frontier, frontier = "both"), type = "l", main = "MV Frontier (monthly returns 1994-2021)", xlab = "Volatility",ylab = "Return", panel.first = grid(), xlim = c(0.001,0.065), ylim = c(0.001, 0.011))
rf <- 0.018/12
slope <- (mv.sharpe.portfolio.ret-rf)/(mv.sharpe.portfolio.sigma)
abline(a = rf, b = slope, lty = 3, col = "black")
points(mv.minvar.portfolio.sigma, mv.minvar.portfolio.ret, col = "red", pch = 15)
points(mv.sharpe.portfolio.sigma, mv.sharpe.portfolio.ret, col = "green", pch = 15)
graphics::legend(x= "topleft", legend=c("Minimum Variance Portfolio", "Maximum Sharpe Portfolio", "Portfolio Frontier", "CAL"),
                 col=c("red", "green", "black", "black"), lty = c(NA, NA, 1, 3), 
                 pch = c(15,15,NA, NA), cex = .8, text.font = 1)
points(getSigma(mv.portfolio.data) %>% diag() %>% sqrt(), getMu(mv.portfolio.data), col = brewer.pal(n = getNAssets(mv.portfolio.data), name = "Paired") , pch = 18)
text(getSigma(mv.portfolio.data) %>% diag() %>% sqrt(), getMu(mv.portfolio.data), labels = gsub(mv.assets, pattern = "_", replacement = " "), cex= 0.8, pos=4)

# Allocation plotting ----

allocation_plotter(mv.sharpe.portfolio, col = c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B", "#231f8c", "#1d125c", "#1d125c"), radius = 0.7, cex = 0.8, box = F)
title(main = "Maximum Sharpe Portfolio")

allocation_plotter(mv.minvar.portfolio, col = c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B", "#231f8c", "#1d125c", "#1d125c"), radius = 0.7, cex = 0.8, box = F)
title(main = "Global Minimum Variance Portfolio")
