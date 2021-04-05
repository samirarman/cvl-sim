logit <- function(x, r, x_base, y_base) {
  x_0 <- x_base - log((1) / y_base  - 1) / -r
  1 / (1 + exp(-r * (x - x_0)))
}

power <- function(x, bids, a, b) {
  bids + a * (x - 2000) ^ b
}

plot(
  NULL,
  NULL,
  xlim = c(150, 300),
  ylim = c(0, 1),
  xlab = "Investimento em publicidade - R$",
  ylab = "Probabilidade de vencer um orçamento"
)

for (i in 1:50) {
  p_base <- 184
  y_base <- rnorm(1, mean = .275, .05)
  r <- sample(c(-0.02, -0.03, -0.02), 1, prob = c(.4, .3, .3))
  
  p <- 150:300
  lines(p,
        logit(p, r, p_base, y_base),
        type = "l",
        col = "grey")
  
}

plot(
  NULL,
  NULL,
  xlim = c(2000, 10000),
  ylim = c(0, 450),
  xlab = "Investimento em publicidade - R$",
  ylab = "Número de pedidos de orçamento"
)

for (i in 1:50) {
  ad_base <- 2000
  bids <- rnorm(1, mean = 135, 30)
  print(bids)
  a <- sample(c(0.15, 0.2, 0.04), 1, prob = c(.4, .3, .3))
  ad <- 2000:10000
  
  lines(ad,
        power(ad, bids, a, 0.7),
        type = "l",
        col = "gray")
  
}

source("./R/functions.R")
qty_per_bid <- read_csv2("./generated-data/qty_per_bid.csv")$x

plot_dist <- function(p, ad, size = 2e5) {
  scen <- gen_series(p, ad, qty_per_bid, "scenario", size)
  rcurve <- gen_series(p, ad, qty_per_bid, "random_curve", size)
  
  
  br_samples <-
    rnorm(size,  power(ad, mean(BR_EST), sum(BR_A * BR_SCENARIO_PROB), 0.7), BR_SD)
  bw_samples <-
    rnorm(size, logit(p, sum(BW_R * BW_SCENARIO_PROB), P_0, mean(BW_EST)), BW_SD)
  qty <- sample(qty_per_bid, size, replace = TRUE)
  q <- br_samples * bw_samples * qty
  print(
    paste(
      scen %>% mean,
      rcurve %>% mean,
      br_samples %>% mean,
      bw_samples %>% mean,
      qty %>% mean,
      q %>% mean
    )
  )
  
  plot(density(rcurve), main = "", xlim = c(-100, 1000))
  lines(density(scen), col = "blue")
  lines(density(q), col = "red")
  abline(v = mean(rcurve), col =
           "black", lty = 2)
  abline(v = mean(scen), col = "blue", lty = 2)
  abline(v = mean(q), col = "red", lty = 2)
  legend(
    400,
    0.007,
    legend = c("Truncagem", "Curvas randomicas", "Distr. Normal"),
    col = c("black", "blue", "red"),
    lty = 1
  )
}

plot_dist(150, 2000)
plot_dist(150, 10000)
plot_dist(184, 2000)
plot_dist(184, 10000)
plot_dist(300, 2000)
plot_dist(300, 10000)

iter <- 20000
bw_vector <- numeric(iter)
bw_base_vector <- numeric(iter)
br_vector <- numeric(iter)
br_base_vector <- numeric(iter)
qty <- numeric(iter)
qty_base <- numeric(iter)
qty_per_bid <- read.csv2("./generated-data/qty_per_bid.csv")$x


# Exanple for p = 200, ad = 5000
for (i in 1:iter) {
  p_base <- 184
  y_base <- rnorm(1, mean = .275, .05)
  r <- sample(c(-0.02, -0.03, -0.02), 1, prob = c(.4, .3, .3))
  bw_vector[i] <- logit(184, r, p_base, y_base)
  bw_base_vector[i] <- logit(184, r, p_base, y_base)
  
  ad_base <- 2000
  bids <- rnorm(1, mean = 135, 30)
  a <- sample(c(0.15, 0.2, 0.04), 1, prob = c(.4, .3, .3))
  ad <- 2000:10000
  
  br_vector[i] <- power(5000, bids, a, 0.7)
  br_base_vector[i] <- power(2000, bids, a, 0.07)
  
  qty[i] <- sample(qty_per_bid, 1) * bw_vector[i] * br_vector[i]
  qty_base[i] <- sample(qty_per_bid, 1) *
    bw_base_vector[i] *
    br_base_vector[i]
}

# P(delta_qty > 0)
plot(density(qty_base), col = "gray")
lines(density(qty), col = "red")
abline(v = mean(qty), col = "red")
abline(v = mean(qty_base), col = "gray")
delta_qty <- qty - qty_base
p_delta_qty <- sum(delta_qty > 0) / length(delta_qty)



mean_delta_qty <- mean(delta_qty)
q_dqty_975 <- quantile(delta_qty, .975)
q_dqty_025 <- quantile(delta_qty, .025)

plot(density(delta_qty),
     col =  "red",
     main = paste0("P(\u0394Q > 0) = ", p_delta_qty))
abline(v = 0, col = "red")
abline(v = q_dqty_025, col = "gray")
abline(v = q_dqty_975, col = "gray")


margin <- qty * 200
margin_base <- qty_base * 184

plot(density(qty_base), col = "gray")
lines(density(qty), col = "red")
abline(v = mean(qty), col = "red")
abline(v = mean(qty_base), col = "gray")
delta_margin <- margin - margin_base
p_delta_margin <- sum(delta_margin > 0) / length(delta_margin)
mean_delta_margin <- mean(delta_margin)
q_dmrgn_975 <- quantile(delta_margin, .975)
q_dmrgn_025 <- quantile(delta_margin, .025)

hist((delta_margin),
     col =  "red",
     xlim = c(q_dmrgn_025, q_dmrgn_975),
     main = paste0("P(\u0394M > 0) = ", p_delta_margin)
)
abline(v = 0, col = "red")
