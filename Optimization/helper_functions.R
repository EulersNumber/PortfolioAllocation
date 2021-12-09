# Notes ----

# Run these functions to your environment before running the other files

# Functions ----

# calculates the covariance matrix from expected correlations and volatilities
cor2cov <- function(Rho, Sigma){
  diag(Sigma) %*% Rho %*% diag(Sigma)
}

# inputs pairwise (idx1, idx2) correlations to a custom correlation matrix
cor_inputer <- function(cor_matrix, idx1, idx2, cor) {
  cor_matrix[idx1, idx2] <- cor
  cor_matrix[idx2, idx1] <- cor
  cor_matrix
}

# plots the allocation imposed by the portfolio-object (fPortfolio)
allocation_plotter <- function (object, pos = NULL, labels = TRUE, col = NULL, box = TRUE, legend = TRUE, radius = 0.8, ...) {
  
  Title <- "Weights"
  if (is.null(col)) 
    col <- seqPalette(getNAssets(object), "Blues")
  if (sum(c(par()$mfrow, par()$mfcol)) == 4) 
    CEX = 0.9
  else CEX = 0.7
  if (is.null(pos)) {
    Weights <- getWeights(object@portfolio)
  }
  else {
    Weights <- getWeights(object@portfolio)[pos, ]
  }
  X <- Weights
  nX <- getNAssets(object)
  Sign <- rep("+", nX)
  Sign[(1:nX)[X < 0]] <- "-"
  absX <- abs(X)
  Index <- (1:nX)[X > 0]
  if (!is.logical(labels)) {
    Names <- pieLabels <- labels
    labels <- FALSE
  }
  else {
    Names <- pieLabels <- object@data@data$names
  }
  col <- col[Index]
  legendAssets <- Names[Index]
  Labels <- paste(Names, Sign)
  Labels = Labels[X > 0]
  Y <- X[X > 0]
  if (labels) {
    pie(Y, labels = str_sub(gsub(Labels, pattern = "_", 
                                 replacement = " "), 1, -3), col = col, radius = radius, 
        cex = CEX)
  }
  else {
    pie(Y, labels = pieLabels, col = col, radius = radius, 
        ...)
  }
  if (labels) {
    mtext(paste(getType(object), "|", getSolver(object)), 
          side = 4, adj = 0, col = "grey", cex = 0.7)
  }
  if (legend) {
    legend("topleft", legend = gsub(legendAssets, pattern = "_", 
                                    replacement = " "), bty = "n", cex = CEX, fill = col)
    legendY <- as.character(round(100 * Y, digits = 1))
    legendY <- paste(Sign[Index], legendY, sep = "")
    legendY <- paste(legendY, "%")
    legend("topright", legend = legendY, bty = "n", cex = CEX, 
           fill = col)
  }
  if (box) 
    box()
  invisible(Y)
  
}
