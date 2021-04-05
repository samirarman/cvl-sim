library(tidyverse)

# Constantes -----
BW_EST <- c(0.25, 0.3)
BR_EST <- c(120, 150)

P_0 <- 184
AD_0 <- 2000
VAR_COST <- c(103, 108.15, 113.3)
VAR_COST_PROB <- c(0.85, 0.10, 0.05)

BW_0 <- mean(BW_EST)
BW_R <- c(-0.02, -0.03, -0.01)
BW_SD <- (BW_EST[2] - BW_EST[1])
BW_SCEN <- c("avg", "sens", "ns")
BW_SCENARIO_PROB <- c(0.4, 0.3, 0.3)

BR_0 <- mean(BR_EST)
BR_A <- c(0.12, 0.15, 0.07)
BR_B <- rep(0.7, 3)
BR_SD <- BR_EST[2] - BR_EST[1]
BR_SCEN <- c("avg", "opt", "pes")
BR_SCENARIO_PROB <- c(0.4, 0.3, 0.3)

# Funções -----
logit <- function(x, r, x_base, y_base) {
  x_0 <- x_base - log((1) / y_base  - 1) / -r
  1 / (1 + exp(-r * (x - x_0)))
}

power <- function(x, base, a, b) {
  base + a * (x - AD_0) ^ b
}

bw <- function(p, type = "avg") {
  r <- BW_R[BW_SCEN == type]
  logit(p, r, P_0, BW_0)
}

bw_hat <- function(p) {
  bw(p) * BW_SCENARIO_PROB[1] +
    bw(p, "sens") * BW_SCENARIO_PROB[2] +
    bw(p, "ns") * BW_SCENARIO_PROB[3]
}

br <- function(ad, type = "avg") {
  a <- BR_A[BR_SCEN == type]
  power(ad, BR_0, a, BR_B[3])
}

br_hat <- function(ad) {
  br(ad) * BR_SCENARIO_PROB[1] +
    br(ad, "opt") * BR_SCENARIO_PROB[2] +
    br(ad, "pes") * BR_SCENARIO_PROB[3]
}

qty_hat <- function(p, ad, qty_per_bid) {
  br_hat(ad) * bw_hat(p) * mean(qty_per_bid)
}

gen_bw_samples <- function(p, size = 1) {
  samples <- rnorm(size, bw_hat(p), BW_SD)
  samples[samples < 0] <- 0
  samples[samples > 1] <- 1
  samples
  
}

gen_br_samples <- function(ad, size = 1) {
  samples <- rnorm(size, br_hat(ad), BR_SD)
  samples[samples < 0] <- 0
  samples
}

gen_qty <- function(br_samples, bw_samples, qty_per_bid, size) {
  orders <- bw_samples * br_samples
  qty_per_bid <- sample(qty_per_bid, size, replace = TRUE)
  orders * qty_per_bid
}

gen_series <- function(p,
                       ad,
                       qty_per_bid,
                       size = 1) {
  qty <- numeric(size)
  
  br_samples <- gen_br_samples(ad, size)
  bw_samples <- gen_bw_samples(p,  size)
  gen_qty(br_samples, bw_samples, qty_per_bid, size)
}

compare_prob <-
  function(p,
           ad,
           qty_per_bid,
           iter = 10,
           size = 50) {
    q1 <- matrix(ncol = iter, nrow = size)
    q2 <- matrix(ncol = iter, nrow = size)
    for (i in 1:iter) {
      q1[, i] <- gen_series(p, ad, qty_per_bid, size)
      q2[, i] <- gen_series(P_0, AD_0, qty_per_bid, size)
    }
    
    rev1 <- q1 * p
    rev2 <- q2 * P_0
    
    var_cost_samples <-
      sample(VAR_COST,
             size,
             replace = TRUE,
             prob = VAR_COST_PROB)
    
    cost1 <- q1 * var_cost_samples
    cost2 <- q2 * var_cost_samples
    
    margin1 <- rev1 - cost1
    margin2 <- rev2 - cost2
    
    profit1 <- margin1 - (ad - AD_0)
    profit2 <- margin2
    
    delta_profit <- profit1 - profit2
    p_delta_profit <- sum(delta_profit > 0) / size
    p_delta_profit <- mean(rowMeans(delta_profit > 0))
    
    delta_qty <- q1 - q2
    p_delta_qty <- mean(rowMeans(delta_qty > 0))
    
    delta_margin <- margin1 - margin2
    p_delta_margin <- mean(rowMeans(delta_margin > 0))
    
    delta_cost <- cost1 - cost2
    p_delta_cost <- mean(rowMeans(delta_cost > 0))
    
    return(
      list(
        "p_delta_margin" = p_delta_margin,
        "p_delta_cost" = p_delta_cost,
        "p_delta_qty" = p_delta_qty,
        "p_delta_profit" = p_delta_profit
      )
    )
  }
