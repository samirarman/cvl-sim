library(tidyverse)

# Constantes -----
BW_EST <- c(0.25, 0.3)
BR_EST <- c(120, 150)

P_0 <- 184
AD_0 <- 2000
VAR_COST <- c(103, 108.15, 113.3)
VAR_COST_PROB <- c(0.85, 0.10, 0.05)

BW_0 <- mean(BW_EST)
BW_R <- c(-0.02,-0.03,-0.01)
BW_SD <- (BW_EST[2] - BW_EST[1])
BW_SCEN <- c("avg", "sens", "ns")
BW_SCENARIO_PROB <- c(0.4, 0.3, 0.3)

BR_0 <- mean(BR_EST)
BR_A <- c(0.15, 0.2, 0.04)
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

add_noise <- function(x, sd) {
  x + rnorm(length(x), 0, sd)
} 

correct_bw_prob <- function(bw) {
  bw[bw < 0]<- 0
  bw[bw > 1] <- 1
  bw
}

correct_bids_req <- function(br) {
  br[br < 0] <- 0
  br
}

bw <- function(p, type = "avg") {
  if (type == "avg") {
    r <- BW_R[1]
  } else if (type == "ns") {
    r <- BW_R[2]
  } else if (type == "sens") {
    r <- BW_R[3]
  }
  logit(p, BW_R[3], P_0, BW_0)
}

bw_hat <- function(p) {
  bw_ans <- c(bw(p), bw(p, "ns"), bw(p, "sens"))
  sum(bw_ans * BW_SCENARIO_PROB)
}

br <- function(ad, type = "avg") {
  if (type == "avg") {
    a <- BR_A[1]
  } else if (type == "opt") {
    a <- BR_A[2]
  } else if (type == "pes") {
    a <- BR_A[3]
  }
  power(ad, BR_0, a, BR_B[3])
}

br_hat <- function(ad) {
  br_ans <- c(br(ad), br(ad, "opt"), br(ad, "pes"))
  sum(br_ans * BR_SCENARIO_PROB)
}


qty_hat <- function(p, ad, qty_per_bid) {
  br_hat(ad) * bw_hat(p) * mean(qty_per_bid)
}

gen_bw_samples <- function(p, gen_types = FALSE, size = 1) {
  bw_samples <- numeric(size)
  if(gen_types) {
      bw_type <- sample(BW_SCEN, size, replace = TRUE, prob = BW_SCENARIO_PROB)
      bw_samples <- map_dbl(bw_type, ~ bw(p, .x)) %>%
        add_noise(BW_SD) %>%
        correct_bw_prob()
  } else {
    y_base <- rnorm(size, BW_0, BW_EST[2] - BW_EST[1])
      y_base[y_base < 0] <- 0
      y_base[y_base > 1] <- 1
    r <- sample(BW_R, size, replace = TRUE, prob = BW_SCENARIO_PROB)
    bw_samples <- logit(rep(p, size), r, P_0, y_base)
  }
  bw_samples
}

gen_br_samples <- function(ad, gen_types = FALSE, size = 1) {
  br_samples <- numeric(size)
  if(gen_types) {
      br_type <- sample(BR_SCEN, size, replace = TRUE, prob = BR_SCENARIO_PROB)
      br_samples <- map_dbl(br_type, ~ br(ad, .x)) %>%
        add_noise(BR_SD) %>%
        correct_bids_req()
  } else {
    y_base <- rnorm(size, BR_0, BR_EST[2] - BR_EST[1])
    a <- sample(BR_A, size, replace = TRUE, prob = BR_SCENARIO_PROB)
    br_samples <- power(rep(ad, size), y_base, a, 0.7)
  }
  br_samples
}

gen_qty <- function(br_samples, bw_samples, qty_per_bid, size) {
  orders <- bw_samples * br_samples
  qty_per_bid <- sample(qty_per_bid, size, replace = TRUE)
  orders * qty_per_bid
}

gen_series <- function(
  p,
  ad,
  qty_per_bid,
  type = c("scenario", "random_curve"),
  size = 1) {
  qty <- numeric(size)

  if (type == "scenario") {
    br_samples <- gen_br_samples(ad, TRUE, size)
    bw_samples <- gen_bw_samples(p, TRUE, size)
    qty <- gen_qty(br_samples, bw_samples, qty_per_bid, size)
  }
  if(type == "random_curve") {
    br_samples <- gen_br_samples(ad, FALSE, size)
    bw_samples <- gen_bw_samples(p, FALSE, size)
    qty <- gen_qty(br_samples, bw_samples, qty_per_bid, size)
  }
  qty
}

compare_prob <-
  function(p,
           ad,
           qty_per_bid,
           type = c("scenario", "random_curve"),
           iter = 10,
           size = 50) {
    q1 <- matrix(ncol = iter, nrow = size)
    q2 <- matrix(ncol = iter, nrow = size)
    for (i in 1:iter) {
      q1[,i] <- gen_series(p, ad, qty_per_bid, type, size)
      q2[,i] <- gen_series(P_0, AD_0, qty_per_bid, type, size)
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
    p_delta_qty <- sum(delta_qty > 0) / size
    
    delta_margin <- margin1 - margin2
    p_delta_margin <- sum(delta_margin > 0) / size
    
    delta_cost <- cost1 - cost2
    p_delta_cost <- sum(delta_cost < 0) / size
    
    return(
      list(
        "p_delta_margin" = p_delta_margin,
        "p_delta_cost" = p_delta_cost,
        "p_delta_qty" = p_delta_qty,
        "p_delta_profit" = p_delta_profit
      )
    )
  }