
summary.robust <- function(model) {
  s <- summary(model)
  X <- model.matrix(model)
  u2 <- residuals(model)^2
  XDX <- 0
  
  for(i in 1:nrow(X)) {
    XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
  }
  
  # inverse(X'X)
  XX1 <- solve(t(X)%*%X)
  
  # Variance calculation 
  varcovar <- XX1 %*% XDX %*% XX1
  
  # degrees of freedom adjustment
  dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
  
  # Standard errors of the coefficient estimates are the
  # square roots of the diagonal elements
  stdh <- dfc*sqrt(diag(varcovar))
  
  t <- model$coefficients/stdh
  p <- 2*pnorm(-abs(t))
  robust.CI.low <- model$coefficients - stdh*qnorm(0.975)
  robust.CI.high <- model$coefficients + stdh*qnorm(0.975)
  
  results <- cbind(model$coefficients, stdh, p, robust.CI.low, robust.CI.high)
  colnames(results) <- list("Estimate","Robust.se", "P_value", "robust.CI.low","robust.CI.high") #dimnames(s$coefficients)
  results
}
