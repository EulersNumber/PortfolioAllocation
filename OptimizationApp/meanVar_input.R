# HELPER FUNCTIONS  ----

# Calculates covariance matrix based on correlation matrix and volatilities
cor2cov <- function(Rho, Sigma){
  diag(Sigma) %*% Rho %*% diag(Sigma)
}

# inputs symmetrical correlations to the correlation matrix
cor_inputer <- function(cor_matrix, idx1, idx2, val) {
  cor_matrix[idx1, idx2] <- val
  cor_matrix[idx2, idx1] <- val
  cor_matrix
}


# DATA SPECIFICATION ----, JP Morgan

options(scipen = 99)

colnames(full_data)
portfolio <- full_data_ts[, c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap",
                              "LT_Treasury", "ST_Treasury", "HY_Corp_Bonds",
                              "REIT", "MSCI_EUR")]
basicStats(portfolio)

assets <- c("US_Large_Cap", "US_Mid_Cap", "US_Small_Cap",
            "LT_Treasury", "ST_Treasury", "HY_Corp_Bonds",
            "REIT", "MSCI_EUR")

rets <- c(0.0516, 0.0565, 0.0617, 
          0.0244, 0.008, 0.0422, 
          0.0675, 0.0917)

vols <- c(0.1502, 0.1706, 0.1961, 
          0.1155, 0.015, 0.0824, 
          0.1511, 0.2156)

nAssets <- length(assets)

cor(portfolio)

cor <- matrix(0, nrow = nAssets, ncol = nAssets, byrow = T)
rownames(cor) <- assets
colnames(cor) <- assets
diag(cor) <- 1
cor

cor <- cor %>%
  cor_inputer(1,2,0.96) %>% 
  cor_inputer(1,3,0.91) %>% 
  cor_inputer(1,4,-0.31) %>%
  cor_inputer(1,5,-0.09) %>%
  cor_inputer(1,6,0.71) %>%
  cor_inputer(1,7,0.73) %>%
  cor_inputer(1,8,0.85) %>%
  cor_inputer(2,3,0.95) %>% 
  cor_inputer(2,4,-0.31) %>%
  cor_inputer(2,5,-0.1) %>%
  cor_inputer(2,6,0.76) %>%
  cor_inputer(2,7,0.76) %>%
  cor_inputer(2,8,0.83) %>%
  cor_inputer(3,4,-0.31) %>%
  cor_inputer(3,5,-0.11) %>%
  cor_inputer(3,6,0.68) %>%
  cor_inputer(3,7,0.73) %>%
  cor_inputer(3,8,0.76) %>%
  cor_inputer(4,5,0.07) %>%
  cor_inputer(4,6,-0.23) %>%
  cor_inputer(4,7,0.02) %>%
  cor_inputer(4,8,-0.28) %>%
  cor_inputer(5,6,-0.12) %>%
  cor_inputer(5,7,-0.06) %>%
  cor_inputer(5,8,-0.02) %>%
  cor_inputer(6,7,0.64) %>%
  cor_inputer(6,8,0.7) %>%
  cor_inputer(7,8,0.65)
cor

covar_mat <- cor2cov(cor, vols)
rownames(covar_mat) <- assets
colnames(covar_mat) <- assets
covar_mat

# JP MORGAN ESTIMATES -----

library(NMOF)
library(RColorBrewer)

minvar.weigths <- minvar(var = covar_mat,
                         groups = list(c(1:3, 8), 4:6),
                         wmin = c(0.1, 0.05, 0.01, 0.1, 0.05, 0.01, 0.05, 0.05),
                         wmax = c(0.3, 0.2, 0.1 , 0.2, 0.15, 0.1, 0.15, 0.2),
                         groups.wmin = c(0.45, 0.25),
                         groups.wmax = c(0.6, 0.4)
                         )

minvar.weigths

optim.weights <- mvPortfolio(m = rets,
                              var = covar_mat,
                              groups = list(c(1:3, 8), 4:6),
                              wmin = c(0.1, 0.05, 0.01, 0.1, 0.05, 0.01, 0.05, 0.05),
                              wmax = c(0.3, 0.2, 0.1 , 0.2, 0.15, 0.1, 0.15, 0.2),
                              groups.wmin = c(0.45, 0.25),
                              groups.wmax = c(0.6, 0.4),
                              min.return = 0.05
                              )
optim.weights

sigma_minvar <- minvar.weigths %*% covar_mat %*% minvar.weigths %>% sqrt() %>% as.numeric
mu_minvar <- sum(minvar.weigths * rets)


sigma_optim <- sharpe.weights %*% covar_mat %*% sharpe.weights %>% sqrt() %>% as.numeric
mu_optim <- sum(sharpe.weights * rets)


frontier <- mvFrontier(m = rets,
                      var = covar_mat,
                      groups = list(c(1:3, 8), 4:6),
                      wmin = c(0.1, 0.05, 0.01, 0.1, 0.05, 0.01, 0.05, 0.05),
                      wmax = c(0.3, 0.2, 0.1 , 0.2, 0.15, 0.1, 0.15, 0.2),
                      groups.wmin = c(0.45, 0.25),
                      groups.wmax = c(0.6, 0.4),
                      n = 100,
                      rf = NA
                      )

sharpe.weights <- frontier$portfolios[,28]
sigma_sharpe <- sharpe.weights %*% covar_mat %*% sharpe.weights %>% sqrt() %>% as.numeric
mu_sharpe <- sum(sharpe.weights * rets)


plot(frontier$volatility, frontier$returns, pch = 20, type = "l",
     main = "MV Optimization, JP Morgan Estimates",xlab = "Expected Volatility",ylab = "Expected Return",
     xlim = c(0.005,0.22), ylim = c(0.,0.1), panel.first = grid())
points(sigma_minvar, mu_minvar, col = "blue", pch = 19)
points(sigma_optim, mu_optim, col = "red", pch = 19)
points(sigma_sharpe, mu_sharpe, col = "green", pch = 19)
tbl <- frontier$returns/frontier$volatility %>% data.frame()
rf <- 0.018/12
slope <- (mu_sharpe-rf)/(sigma_sharpe)
abline(a = rf, b = slope, lty = 3, col = "black")
graphics::legend(x= "topleft", legend=c("Minimum Variance Portfolio", "Target return (5%) Portfolio", "Maximum Sharpe Portfolio", "Efficient Frontier", "CML"),
                 col=c("blue", "red", "green", "black"), lty = c(NA, NA, NA, 1, 3), pch = c(19,19,19,NA, NA), cex = .8, text.font = 1)
points(vols, rets, col = brewer.pal(n = nAssets, name = "Paired") , pch = 18)
text(vols, rets, labels = assets, cex= 0.7, pos=1)


# INVESCO ESITMATES ----

assets
rets.invesco <- c(0.0760, 0.0890, 0.1120, 0.0160, 0.0080,
                  0.0330, 0.0970, 0.0810)

vols.invesco <- c(0.1680, 0.1970, 0.2310, 0.1170, 0.0150,
                  0.1010, 0.1870, 0.1880)

cor.invesco <- read.csv("Invesco.csv")
cor.invesco <- cor.invesco[,2:ncol(cor.invesco)]
rownames(cor.invesco) <- assets
colnames(cor.invesco) <- assets
cor.invesco <- as.matrix(cor.invesco)

covar_mat.invesco <- cor2cov(Rho = cor.invesco, Sigma = vols.invesco)
rownames(covar_mat.invesco) <- assets
colnames(covar_mat.invesco) <- assets
covar_mat.invesco

minvar.weigths.invesco <- minvar(var = covar_mat.invesco,
                                 groups = list(c(1:3, 8), 4:6),
                                 wmin = c(0.1, 0.05, 0.01, 0.1, 
                                          0.05, 0.01, 0.05, 0.05),
                                 wmax = c(0.3, 0.2, 0.1 , 0.2,
                                          0.15, 0.1, 0.15, 0.2),
                                 groups.wmin = c(0.45, 0.25),
                                 groups.wmax = c(0.6, 0.4)
                                 )

minvar.weigths.invesco

optim.weights.invesco <- mvPortfolio(m = rets,
                                     var = covar_mat.invesco,
                                     groups = list(c(1:3, 8), 4:6),
                                     wmin = c(0.1, 0.05, 0.01, 0.1,
                                              0.05, 0.01, 0.05, 0.05),
                                     wmax = c(0.3, 0.2, 0.1 , 0.2,
                                              0.15, 0.1, 0.15, 0.2),
                                     groups.wmin = c(0.45, 0.25),
                                     groups.wmax = c(0.6, 0.4),
                                     min.return = 0.05
                                     )
optim.weights.invesco

sigma_minvar.invesco <- minvar.weigths.invesco %*% 
  covar_mat.invesco %*% 
  minvar.weigths.invesco %>% 
  sqrt() %>% 
  as.numeric

mu_minvar.invesco <- sum(minvar.weigths.invesco * rets.invesco)


sigma_optim.invesco <- optim.weights.invesco %*% 
  covar_mat.invesco %*% 
  optim.weights.invesco %>% 
  sqrt() %>% 
  as.numeric

mu_optim.invesco <- sum(optim.weights.invesco * rets.invesco)


frontier.invesco <- mvFrontier(m = rets.invesco,
                               var = covar_mat.invesco,
                               groups = list(c(1:3, 8), 4:6),
                               wmin = c(0.1, 0.05, 0.01, 0.1, 
                                        0.05, 0.01, 0.05, 0.05),
                               wmax = c(0.3, 0.2, 0.1 , 0.2, 
                                        0.15, 0.1, 0.15, 0.2),
                               groups.wmin = c(0.45, 0.25),
                               groups.wmax = c(0.6, 0.4),
                               n = 100,
                               rf = NA
)

sharpe.weights.invesco <- frontier.invesco$portfolios[,27]

sigma_sharpe.invesco <- sharpe.weights.invesco %*% 
  covar_mat.invesco %*% 
  sharpe.weights.invesco %>% 
  sqrt() %>% 
  as.numeric

mu_sharpe.invesco <- sum(sharpe.weights.invesco * rets.invesco)


plot(frontier.invesco$volatility, frontier.invesco$returns, type = "l",
     main = "MV Optimization, Invesco Estimates",xlab = "Expected Volatility",ylab = "Expected Return",
     xlim = c(0.005,0.205), ylim = c(0.,0.1), panel.first = grid())
points(sigma_minvar.invesco, mu_minvar.invesco, col = "blue", pch = 19)
points(sigma_optim.invesco, mu_optim.invesco, col = "red", pch = 19)
points(sigma_sharpe.invesco, mu_sharpe.invesco, col = "green", pch = 19)
rf <- 0.018/12
slope <- (mu_sharpe.invesco-rf)/(sigma_sharpe.invesco)
abline(a = rf, b = slope, lty = 3, col = "black")
graphics::legend(x= "topleft", legend=c("Minimum Variance Portfolio", "Target return (5%) Portfolio", "Maximum Sharpe Portfolio", "Efficient Frontier", "CML"),
                 col=c("blue", "red", "green", "black"), lty = c(NA, NA, NA, 1, 3), 
                 pch = c(19,19,19,NA, NA), cex = .8, text.font = 1)
points(vols.invesco, rets.invesco, col = brewer.pal(n = nAssets, name = "Paired") , pch = 18)
text(vols.invesco, rets.invesco, labels = assets, cex= 0.7, pos=1)