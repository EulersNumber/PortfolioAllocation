# Library imports ----

library(NMOF)
library(RColorBrewer)

# Portfolio specification ----

jpm.assets <- c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap", "LT_Treasury", "IMT_Treasury", "ST_Treasury", "HY_Corp_Bonds", "REIT") %>% gsub(pattern = "_", replacement = " ")

# Data specification ----

jpm.rets <- c(0.0516, 0.0565, 0.0617, 
              0.0244, 0.0214, 0.0130, 0.0422, 
              0.0675)

jpm.vols <- c(0.1502, 0.1706, 0.1961,
              0.1155, 0.0281,0.0042, 0.0824, 
              0.1511)

rf <- 0.018/12 # US30T yield 

jpm.cor <- read.csv2("JPM_corr.csv")
jpm.cor <- jpm.cor[,2:ncol(jpm.cor)]
jpm.cor <- as.matrix(jpm.cor)

jpm.cov <- cor2cov(jpm.cor, jpm.vols)
rownames(jpm.cov) <- jpm.assets
colnames(jpm.cov) <- jpm.assets

jpm.cov # full covariance matrix
jpm.cov %>% diag() %>% sqrt() # expected volatilities
jpm.cov %>% diag() %>% sqrt() - jpm.vols # should be zero if computed correctly

# Optimization ----

jpm.frontier <- mvFrontier(m = jpm.rets,
                           var = jpm.cov,
                           groups = list(c(1:3), 4:7),
                           wmin = c(0.05, 0.025, 0.01, 0, 0, 0, 0, 0.025),
                           wmax = c(0.3, 0.25, 0.2, 0.3, 0.3, 0.3, 0.1, 0.2),
                           groups.wmin = c(0.4, 0.25),
                           groups.wmax = c(0.6, 0.4),
                           n = 100,
                           rf = NA)


jpm.minvar.weigths <- minvar(var = jpm.cov, 
                             groups = list(c(1:3), 4:7),
                             wmin = c(0.05, 0.025, 0.01, 0, 0, 0, 0, 0.025),
                             wmax = c(0.3, 0.25, 0.2, 0.3, 0.3, 0.3, 0.1, 0.2),
                             groups.wmin = c(0.4, 0.25),
                             groups.wmax = c(0.6, 0.4))


jpm.target.weights <- mvPortfolio(m = jpm.rets,
                                  var = jpm.cov,
                                  groups = list(c(1:3), 4:7),
                                  wmin = c(0.05, 0.025, 0.01, 0, 0, 0, 0, 0.025),
                                  wmax = c(0.3, 0.25, 0.2, 0.3, 0.3, 0.3, 0.1, 0.2),
                                  groups.wmin = c(0.4, 0.25),
                                  groups.wmax = c(0.6, 0.4),
                                  min.return = 0.05
)

jpm.minvar.sigma <- sqrt(jpm.minvar.weigths %*% jpm.cov %*% jpm.minvar.weigths) %>% as.numeric
jpm.minvar.mu <- sum(jpm.minvar.weigths * jpm.rets)

jpm.target.sigma <- sqrt(jpm.target.weights %*% jpm.cov %*% jpm.target.weights) %>% as.numeric
jpm.target.mu <- sum(jpm.target.weights * jpm.rets)

jpm.sharpe.index <- which.max((jpm.frontier$returns - rf) / jpm.frontier$volatility) # index of the portfolio with highest Sharpe ratio
jpm.sharpe.weights <- jpm.frontier$portfolios[, jpm.sharpe.index]
jpm.sharpe.sigma <- sqrt(jpm.sharpe.weights %*% jpm.cov %*% jpm.sharpe.weights) %>% as.numeric()
jpm.sharpe.mu <- sum(jpm.sharpe.weights * jpm.rets)

# export maximum Sharpe allocation to Excel for plotting (optional)
jpm.sharpe.weights.df <- data.frame(jpm.assets, jpm.sharpe.weights)
colnames(jpm.sharpe.weights.df) <- c("Asset","Weight")
write.xlsx(jpm.sharpe.weights.df[order(jpm.sharpe.weights.df$Weight, decreasing = T),], "input_JPM_sharpe_weights.xlsx", rownames = F)

# export Minimum Variance allocation to Excel for plotting (optional)
jpm.minvar.weights.df <- data.frame(jpm.assets, jpm.minvar.weigths)
colnames(jpm.minvar.weights.df) <- c("Asset","Weight")
write.xlsx(jpm.minvar.weights.df[order(jpm.minvar.weights.df$Weight, decreasing = T),], "input_JPM_minvar_weights.xlsx", rownames = F)

# Efficient frontier plotting ----

# graphical parameters must be adjusted for own needs (e.g., xlim and ylim)
plot(jpm.frontier$volatility, jpm.frontier$returns, pch = 20, type = "l", main = "MV Frontier, JP Morgan Annual Estimates",xlab = "Expected Volatility",ylab = "Expected Return", panel.first = grid(), xlim = c(0,0.24), ylim = c(0.,0.11))
slope <- (jpm.sharpe.mu-rf)/(jpm.sharpe.sigma)
abline(a = rf, b = slope, lty = 3, col = "black")
points(jpm.minvar.sigma, jpm.minvar.mu, col = "blue", pch = 15)
points(jpm.sharpe.sigma, jpm.sharpe.mu, col = "green", pch = 15)
graphics::legend(x= "topleft", legend=c("Minimum Variance Portfolio", "Maximum Sharpe Portfolio", "Efficient Frontier", "CAL"), col=c("blue", "green", "black", "black"), lty = c(NA, NA, 1, 3), pch = c(15,15,NA, NA), cex = .8, text.font = 1)
points(jpm.vols, jpm.rets, col = brewer.pal(n = nAssets, name = "Paired") , pch = 18)
text(jpm.vols, jpm.rets, labels = gsub(mv.assets, pattern = "_", replacement = " "), cex= 0.7, pos=1)
