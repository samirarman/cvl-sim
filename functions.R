library(tidyverse)
source("./R/parameters.R")

logit <- function(x, c, d, r, x_base, y_base) {
  e <- x_base - log((d - c) / (y_base - c) - 1) / -r
  c + ((d - c) / (1 + exp(-r * (x - e))))
}

bw_avg <- function(p) {
  return(logit(p, BW_C, BW_D, BW_R, P_0, BW_0))
}

bw_ns <- function(p) {
  return(logit(p, BW_C, BW_D, BW_R * (1 - BW_MULT), P_0, BW_0))
}

bw_sens <- function(p) {
  return(logit(p, BW_C, BW_D, BW_R * (1 + BW_MULT), P_0, BW_0))
}

bw <- function(p, type = "avg") {
  ans <- NULL
  
  if (type == "avg") {
    ans <- bw_avg(p)
  } else if (type == "ns") {
    ans <- bw_ns(p)
  } else if (type == "sens") {
    ans <- bw_sens(p)
  }
  
  ans
}

br_avg <- function(ad) {
  return(logit(ad, BR_C, BR_D , BR_R, AD_0, BR_0))
}

br_opt <- function(ad) {
  return(logit(ad, BR_C, BR_D, BR_R * (1 + BR_MULT), AD_0, BR_0))
}

br_pes <- function(ad) {
  return(logit(ad, BR_C, BR_D, BR_R * (1 - BR_MULT), AD_0, BR_0))
}

br <- function(ad, type = "avg") {
  ans <- NULL
  
  if (type == "avg") {
    ans <- br_avg(ad)
  } else if (type == "opt") {
    ans <- br_opt(ad)
  } else if (type == "pes") {
    ans <- br_pes(ad)
  }
  
  ans
}

add_noise <- function(x, sd) {
  x + rnorm(length(x), 0, sd)
  
}

correct_bw_prob <- function(x) {
  x[x < 0] <- 0
  x[x > 1] <- 1
  x
}

correct_bids_req <- function(x) {
  x[x < 0] <- 0
  x
  # ifelse(x < 0, 0, x)
}

gen_series <-
  function(p,
           ad,
           bw_type,
           br_type,
           size = 1000,
           qty_vector,
           method = c("estimated", "partial", "full")) {
    if (method %in% c("estimated", "partial", "full") == FALSE)
      stop("Method not recognized.")
    
    if (method == "estimated")
      size = 1
    
    bw_samples <- map_dbl(rep(bw_type, size), ~ bw(p, .x))
    br_samples <- map_dbl(rep(br_type, size) , ~ br(ad, .x))
    
    if (method == "full") {
      bw_samples <- bw_samples %>% add_noise(BW_SD) %>% correct_bw_prob()
      br_samples <-
        br_samples %>% add_noise(BR_SD) %>% correct_bids_req()
    }
    
    orders <- bw_samples * br_samples
    
    
    if (method == "estimated") {
      qty_sold <- orders * mean(qty_vector)
    } else {
      qty_sold <-
        orders * sample(qty_vector, size, replace = TRUE)
    }
    
    qty_sold
    
  }

comp_prob <-
  function(p1,
           ad1,
           p2,
           ad2,
           bw_type,
           br_type,
           qty_vector,
           size = 50,
           method = "full") {
    q1 <-
      gen_series(p1, ad1, bw_type, br_type, size, qty_vector, method = method)
    q2 <-
      gen_series(p2, ad2, bw_type, br_type, size, qty_vector, method = method)
    
    
    rev1 <- q1 * p1
    rev2 <- q2 * p2
    
    cost1 <- q1 * VAR_COST
    cost2 <- q2 * VAR_COST
    
    margin1 <- rev1 - cost1
    margin2 <- rev2 - cost2
    
    delta_qty <- q1 - q2
    p_delta_qty <- sum(delta_qty < 0) / size
    
    delta_margin <- margin1 - margin2
    p_delta_margin <- sum(delta_margin > 0) / size
    
    
    delta_cost <- cost1 - cost2
    p_cost <- sum(delta_cost < 0) / size
    
    return(
      list(
        "p_delta_margin" = p_delta_margin,
        "p_cost" = p_cost,
        "p_delta_qty" = p_delta_qty
      )
    )
    
  }

mc <-
  function(p1,
           ad1,
           p2,
           ad2,
           bw_type,
           br_type,
           qty_vector,
           size = 50,
           iter = 100,
           ret_type = "mean",
           method = "full") {
    p_delta_margin <- numeric(iter)
    p_delta_cost <- numeric(iter)
    p_delta_qty <- numeric(iter)
    res <- numeric(size)
    
    for (i in 1:iter) {
      res <-
        comp_prob(p1, ad1, p2, ad2, bw_type, br_type, qty_vector, size, method)
      p_delta_margin[i] <- res[[1]]
      p_delta_cost[i] <- res[[2]]
      p_delta_qty[i] <- res[[3]]
    }
    
    result <- NULL
    if (ret_type == "mean") {
      result <- list(
        "p_delta_margin" = mean(p_delta_margin),
        "p_delta_cost" = mean(p_delta_cost),
        "p_delta_qty" = mean(p_delta_qty)
      )
    } else {
      result <- list(
        "p_delta_margin" = p_delta_margin,
        "p_delta_cost" = p_delta_cost,
        "p_delta_qty" = p_delta_qty
      )
    }
    
    result
  }

plot_convergence <- function(parameter, ...) {
  plot(dplyr::cummean(parameter),
       type = "l",
       ylim = c(0, 1),
       ...)
  abline(h = mean(parameter),
         col = "red")
  
}

eval_sim <- function(base_qty, qty_per_bid, method) {
  sim_qty <-
    gen_series(P_0,
               AD_0,
               "avg",
               "avg",
               size = 1000,
               qty_per_bid,
               method = {
                 {
                   method
                 }
               })
  plot(density(sim_qty),
       xlim = c(0, max(max(sim_qty), max(base_qty))),
       ylim = c(0, max(max(
         density(sim_qty)$y
       ), max(
         density(base_qty)$y
       ))))
  lines(density(base_qty), col = "blue", lty = 2)
}



convergence_time <-
  function(size = 1,
           iter = 1000,
           print = TRUE,
           method = "full") {
    start <- Sys.time()
    conv <-
      mc(200,
         4000,
         185,
         2000,
         "avg",
         "avg",
         qty_per_bid,
         size,
         iter,
         "series",
         method)
    end <- Sys.time()
    t <- end - start
    units(t) <- "secs"
    delta_time <- as.numeric(t)
    
    if (print) {
      fig_title = paste0("N = ", size, "\nI = ", iter, "\nt = ", delta_time)
      par(mfrow = c(1, 2), pty = "s")
      plot_convergence(
        conv$p_delta_margin,
        main = paste0(fig_title, "\nvar = ", var(conv$p_delta_margin)),
        ylab = "p(\u0394M > 0)",
        xlab = "I",
        cex.main = 0.8
      )
      plot_convergence(
        conv$p_delta_cost,
        main = paste0(fig_title, "\nvar = ", var(conv$p_delta_cost)),
        ylab = "p(\u0394C < 0)",
        xlab = "I",
        cex.main = 0.8
      )
      print(t)
      
      par(mfrow = c(1, 1))
    }
    
    summary <- tibble(
      size,
      iter,
      delta_time,
      var_p_delta_margin = conv$p_delta_margin %>% cummean %>% var,
      var_p_delta_cost = conv$p_delta_cost %>% cummean %>% var
    )
    
    return(
      list(
       summary = summary,
        p_delta_margin_series = conv$p_delta_margin,
        p_delta_cost_series = conv$p_delta_cost
      )
    )
  }