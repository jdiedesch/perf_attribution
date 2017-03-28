
generateSampleData <- function(){

  # random port weights
  wt1 <- runif(36, min = 0.25, max = 0.35)
  wt2 <- runif(36, min = 0.40, max = 0.50)
  wt3 <- 1 - (wt1 + wt2)
  port_wts <- data.frame(wt1, wt2, wt3)
  rm("wt1", "wt2", "wt3")

  # benchmark weights
  wt1 <- rep(0.3, 36)
  wt2 <- rep(0.45, 36)
  wt3 <- rep(.25, 36)
  bench_wts <- data.frame(wt1, wt2, wt3)
  rm("wt1", "wt2", "wt3")

  # building the sample
  vols <- c(0.12, 0.2, 0.09)
  rho <- matrix(0, nrow = 3, ncol = 3)
  diag(rho) <- 1
  rho[1,2] <- 0.3
  rho[1,3] <- 0.65
  rho[2,3] <- 0.5
  rho2 <- t(rho)
  rho3 <- rho + rho2
  diag(rho3) <- diag(rho3) - 1
  rho <- rho3
  rm("rho2", "rho3")

  covmat <- vols %*% t(vols) * rho
  benchr <- MASS::mvrnorm(36, rep(0,3), covmat)
  portr <- benchr + matrix(rnorm(n = 36 * 3, mean = 0, sd = 0.02), ncol = 3)

  tot_port <- apply(port_wts * portr, 1, sum)

  # add a little random noise to total port
  err_term <- rnorm(n = 36, mean = 0, sd = 0.0001)
  tot_port <- tot_port + err_term
  tot_bench <- apply(bench_wts * benchr, 1, sum)

  colnames(benchr) <- paste("bench", 1:3)
  colnames(portr) <- paste("port", 1:3)

  dataList <- list(portwt = port_wts,
                   benchwt = bench_wts,
                   benchr = benchr,
                   portr = portr,
                   tot_port = tot_port,
                   tot_bench = tot_bench,
                   err_term = err_term)
  return(dataList)
}

