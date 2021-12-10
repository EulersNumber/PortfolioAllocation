# Library imports ----

library(NMOF)
library(RColorBrewer)

# Portfolio specification ----

invesco.assets <- c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap", "LT_Treasury", "IMT_Treasury", "ST_Treasury", "HY_Corp_Bonds", "REIT") %>% gsub(pattern = "_", replacement = " ")

# INVESCO ESITMATES ----

invesco.rets <- c(0.076, 0.089, 0.112, 
                  0.016, 0.014, 0.008, 0.033, 
                  0.097)

invesco.vols <- c(0.168, 0.197, 0.231, 
                  0.117, 0.045, 0.015, 0.101,
                  0.187)

invesco.cor <- read.csv2("Invesco_corr.csv")
invesco.cor <- invesco.cor[,2:ncol(invesco.cor)]
invesco.cor <- as.matrix(invesco.cor)

invesco.cov <- cor2cov(Rho = invesco.cor, Sigma = invesco.vols)
rownames(invesco.cov) <- invesco.assets
colnames(invesco.cov) <- invesco.assets


invesco.cov # full covariance matrix
invesco.cov %>% diag() %>% sqrt() # expected volatilities
invesco.cov %>% diag() %>% sqrt() - invesco.vols # should be zero if computed correctly

# Optimization ----

invesco.frontier <- mvFrontier(m = invesco.rets,
                               var = invesco.cov,
                               groups = list(c(1:3), 4:7),
                               wmin = c(0.05, 0.025, 0.01, 0, 0, 0, 0, 0.025),
                               wmax = c(0.3, 0.25, 0.2, 0.3, 0.3, 0.3, 0.1, 0.2),
                               groups.wmin = c(0.4, 0.25),
                               groups.wmax = c(0.6, 0.4),
                               n = 100,
                               rf = NA)

invesco.minvar.weights <- minvar(var = invesco.cov,
                                 groups = list(c(1:3), 4:7),
                                 wmin = c(0.05, 0.025, 0.01, 0, 0, 0, 0, 0.025),
                                 wmax = c(0.3, 0.25, 0.2, 0.3, 0.3, 0.3, 0.1, 0.2),
                                 groups.wmin = c(0.4, 0.25),
                                 groups.wmax = c(0.6, 0.4))

invesco.target.weights <- mvPortfolio(m = invesco.rets,
                                      var = invesco.cov,
                                      groups = list(c(1:3), 4:7),
                                      wmin = c(0.05, 0.025, 0.01, 0, 0, 0, 0, 0.025),
                                      wmax = c(0.3, 0.25, 0.2, 0.3, 0.3, 0.3, 0.1, 0.2),
                                      groups.wmin = c(0.4, 0.25),
                                      groups.wmax = c(0.6, 0.4),
                                      min.return = 0.07)

invesco.minvar.sigma <- sqrt(invesco.minvar.weights %*% invesco.cov %*% invesco.minvar.weights) %>% as.numeric
invesco.minvar.mu <- sum(invesco.minvar.weights * invesco.rets)

invesco.target.sigma <- sqrt(invesco.target.weights %*% invesco.cov %*% invesco.target.weights) %>% as.numeric
invesco.target.mu <- sum(invesco.target.weights * invesco.rets)

invesco.sharpe.index <- which.max((invesco.frontier$returns - rf) / invesco.frontier$volatility) # index of the portfolio with highest Sharpe ratio
invesco.sharpe.weights <- invesco.frontier$portfolios[, invesco.sharpe.index]
invesco.sharpe.sigma <- sqrt(invesco.sharpe.weights %*% invesco.cov %*% invesco.sharpe.weights) %>% as.numeric()
invesco.sharpe.mu <- sum(invesco.sharpe.weights * invesco.rets)

# export maximum Sharpe allocation to Excel for plotting (optional)
invesco.sharpe.weights.df <- data.frame(invesco.assets, invesco.sharpe.weights)
colnames(invesco.sharpe.weights.df) <- c("Asset","Weight")
write.xlsx(invesco.sharpe.weights.df[order(invesco.sharpe.weights.df$Weight, decreasing = T),], "input_Invesco_sharpe_weights.xlsx", rownames = F)

# export Minimum Variance allocation to Excel for plotting (optional)
invesco.minvar.weights.df <- data.frame(invesco.assets, invesco.minvar.weights)
colnames(invesco.minvar.weights.df) <- c("Asset","Weight")
write.xlsx(invesco.minvar.weights.df[order(invesco.minvar.weights.df$Weight, decreasing = T),], "input_Invesco_minvar_weights.xlsx", rownames = F)

# Efficient frontier plotting ----

# graphical parameters must be adjusted for own needs (e.g., xlim and ylim)
plot(invesco.frontier$volatility, invesco.frontier$returns, pch = 20, type = "l", main = "MV Frontier, Invesco Annual Estimates",xlab = "Expected Volatility",ylab = "Expected Return", panel.first = grid(), xlim = c(0,0.24), ylim = c(0.,0.11))
slope <- (invesco.sharpe.mu-rf)/(invesco.sharpe.sigma)
abline(a = rf, b = slope, lty = 3, col = "black")
points(invesco.minvar.sigma, invesco.minvar.mu, col = "blue", pch = 15)
points(invesco.sharpe.sigma, invesco.sharpe.mu, col = "green", pch = 15)
graphics::legend(x= "topleft", legend=c("Minimum Variance Portfolio", "Maximum Sharpe Portfolio", "Efficient Frontier", "CAL"), col=c("blue", "green", "black", "black"), lty = c(NA, NA, 1, 3), pch = c(15,15,NA, NA), cex = .8, text.font = 1)
points(invesco.vols, invesco.rets, col = brewer.pal(n = nAssets, name = "Paired") , pch = 18)
text(invesco.vols, invesco.rets, labels = gsub(mv.assets, pattern = "_", replacement = " "), cex= 0.7, pos=1)
