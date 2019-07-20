function (lambda, n, beta = 0, K = 3, w = rep(1, K), Pi = rep(1,K)/K, rho = 0, simple = TRUE, power = TRUE, alpha = 5, degree.seed = NULL) 
{
  P0 <- diag(w)
  if (beta > 0) {
    P0 <- matrix(1, K, K)
    diag(P0) <- w/beta
  }
  
  Pi.vec <- matrix(Pi, ncol = 1)
  P <- lambda * P0/((n - 1) * as.numeric(t(Pi.vec) %*% P0 %*% Pi.vec) * (rho * 0.2 + (1 - rho))^2)
  if ((rho > 0) && (!simple) && (!power)) {
    P <- lambda * P0/((n - 1) * as.numeric(t(Pi.vec) %*% P0 %*% Pi.vec) * (0.6)^2)
  }
  
  if ((rho > 0) && (!simple) && (power)) {
    P <- lambda * P0/((n - 1) * as.numeric(t(Pi.vec) %*% P0 %*% Pi.vec) * ((1.285)^2))
  }
  
  M <- matrix(0, n, K)
  membership <- sample(x = K, size = n, replace = TRUE, prob = Pi)
  M[cbind(1:n, membership)] <- 1
  A.bar <- M %*% P %*% t(M)
  node.degree <- rep(1, n)
  if (rho > 0) {
    if (simple) {
      node.degree[runif(n) < rho] <- 0.2
    }
    
    else {
      if (power == FALSE) {
        node.degree <- runif(n) * 0.8 + 0.2
      }
      else {
        MM <- ceiling(n/300)
        if (is.null(degree.seed)) {
          degree.seed <- rplcon(300, 1, alpha)
        }
        node.degree <- sample(degree.seed, size = n, replace = TRUE)
      }
    }
  }
  DD <- diag(node.degree)
  A.bar <- DD %*% A.bar %*% DD
  A.bar <- A.bar * lambda/mean(colSums(A.bar))
  upper.index <- which(upper.tri(A.bar))
  upper.p <- A.bar[upper.index]
  upper.u <- runif(n = length(upper.p))
  upper.A <- rep(0, length(upper.p))
  upper.A[upper.u < upper.p] <- 1
  A <- matrix(0, n, n)
  A[upper.index] <- upper.A
  A <- A + t(A)
  diag(A) <- 0
  return(list(A = A, g = membership, P = A.bar, theta = node.degree))
}