# Created by NJG on Tue Nov  1 11:19:38 2016

  N <- 61 # re-run with N <- 62
  m <- 1
  S <- 0.444
  Z <- 1e-4 
  
  # Globals
  p0 <- 1
  p  <- 1
  L  <- 0
  
  for (k in 1:N) {
    p <- p * (N - k + 1) * S / Z
    
    if (k <= m) {
      p <- p / k
    } else {
      p <- p / m
    }
    
    p0 <- p0 + p
    
    if (k > m) {
      L <- L + (p * (k - m))
    }
  }
  
  p0 <- 1 / p0
  L  <- L * p0
  W  <- L * (S + Z) / (N - L)
  R  <- W + S
  X  <- N / (R + Z)
  U  <- X * S
  u  <- U / m
  Q  <- X * R

  cat("\n")
  cat(sprintf("  M/M/%d/%d/%d repairmen model\n", m, N, N))
  cat(sprintf("  ---------------------------------\n"))
  cat(sprintf("  Machine pop:      %10d\n",   N))
  cat(sprintf("  MT to failure:    %10.4f\n", Z))
  cat(sprintf("  Service time:     %10.4f\n", S))
  cat(sprintf("  Breakage rate:    %10.4f\n", 1 / Z))
  cat(sprintf("  Service rate:     %10.4f\n", 1 / S))
  cat(sprintf("  Utilization:      %10.4f\n", U))
  cat(sprintf("  Per Server:       %10.4f\n", u))
  cat("\n")
  cat(sprintf("  No. in system:    %10.4f\n", Q))
  cat(sprintf("  No in service:    %10.4f\n", U))
  cat(sprintf("  No.  enqueued:    %10.4f\n", Q - U))
  cat(sprintf("  Waiting time:     %10.4f\n", W))
  cat(sprintf("  Throughput:       %10.4f\n", X))
  cat(sprintf("  Response time:    %10.4f\n", R))
  cat(sprintf("  Stretch factor:   %10.4f\n", R / S))


