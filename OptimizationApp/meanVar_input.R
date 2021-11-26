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

# OPTIMIZATION ----

library(NMOF)

minvar.weigths <- minvar(var = covar_mat,
                         groups = list(c(1:3, 8), 4:6),
                         wmin = c(0.1, 0.05, 0.01, 0.1, 0.05, 0.01, 0.05, 0.05),
                         wmax = c(0.3, 0.2, 0.1 , 0.2, 0.15, 0.1, 0.15, 0.2),
                         groups.wmin = c(0.45, 0.25),
                         groups.wmax = c(0.6, 0.4)
                         )

minvar.weigths

sharpe.weights <- mvPortfolio(m = rets,
                              var = covar_mat,
                              groups = list(c(1:3, 8), 4:6),
                              wmin = c(0.1, 0.05, 0.01, 0.1, 0.05, 0.01, 0.05, 0.05),
                              wmax = c(0.3, 0.2, 0.1 , 0.2, 0.15, 0.1, 0.15, 0.2),
                              groups.wmin = c(0.45, 0.25),
                              groups.wmax = c(0.6, 0.4),
                              min.return = 0.05)
sharpe.weights

sigma <- sharpe.weights %*% covar_mat %*% sharpe.weights %>% sqrt() %>% as.numeric
mu <- sum(sharpe.weights * rets)

frontier <- mvFrontier(m = rets,
                        var = covar_mat,
                        groups = list(c(1:3, 8), 4:6),
                        wmin = c(0.1, 0.05, 0.01, 0.1, 0.05, 0.01, 0.05, 0.05),
                        wmax = c(0.3, 0.2, 0.1 , 0.2, 0.15, 0.1, 0.15, 0.2),
                        groups.wmin = c(0.45, 0.25),
                        groups.wmax = c(0.6, 0.4),
                        n = 100)
plot(frontier$volatility, frontier$returns, pch = 19, cex = .75, type = "o",
     main = "Efficient frontier",xlab = "Expected volatility",ylab = "Expected return")

points(sigma, mu, col = "red", pch = 17)


# fPORTFOLIO OPTIMIZATION ----

momentargs <- list()
momentargs$mu <- rets
momentargs$Sigma <- covar_mat
pfolio <- portfolioData(data = momentargs, spec = combinedConstraints)
portfolioSt

covtEstimator <- function (x, spec = NULL, ...) {
  x.mat = as.matrix(x)
  list(mu = rets, Sigma = covar_mat) 
}

defaultSpec <- portfolioSpec()
setEstimator(defaultSpec) <- "covtEstimator"
setTargetReturn(defaultSpec) <- 0.05

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

myPort <- efficientPortfolio(pfolio, defaultSpec, constraints = combinedConstraints)

points(myPort@portfolio@portfolio$targetRisk[2], myPort@portfolio@portfolio$targetReturn[2], col = "red", pch = 17)

frontierTest <- portfolioFrontier(pfolio, defaultSpec)
tailoredFrontierPlot(frontierTest)

?portfolioFrontier

# INVESCO ESITMATES ----

invesco_cor <- read.csv("Invesco.csv")
invesco_cor <- invesco_cor[,2:ncol(invesco_cor)]
rownames(invesco_cor) <- assets
colnames(invesco_cor) <- assets
invesco_cor
