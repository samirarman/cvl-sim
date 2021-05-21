library(tidyverse)
library(Rcpp)
library(RcppDist)
library(parallel)
library(pbmcapply)
sourceCpp("functions.cpp")

p <- 150:300
ad <- seq(2000, 10000, 1000)
qw_scen <- c(1, 2, 3)
qr_scen <- c(1, 2, 3)
grid <- expand_grid(p, ad, qw_scen, qr_scen)

# Estimativas -----
quotes_won <- grid %>%
  pmap_dbl( ~ quotes_won(..1, ..3))
quotes_received <- grid %>%
  pmap_dbl( ~ quotes_received(..2, ..4))
qty <- quotes_received * quotes_won * 14.0202

estimates <-
  tibble(grid, quotes_won, quotes_received, qty) %>%
  mutate(
    rev = qty * p,
    cost = qty * 104.8025,
    drev = rev - rev[p == 185 & ad == 2000],
    dcost = cost - cost[p == 185 & ad == 2000],
    dprofit_gross = drev - dcost - (ad - 2000)
  )

# Simulação ------
cores <- detectCores()
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
mc.reset.stream()
res <- pbmcmapply(
  dtax,
  estimates$drev,
  iter = 15000,
  mc.cores = cores,
  ignore.interactive = TRUE
)

# Resultados ------
names <- dimnames(res)[[1]]
r <- as.data.frame(matrix(unlist(t(res)), nrow = nrow(grid)))
colnames(r) <- names

estimates$dtax <- r$dtax
estimates$p_out <- r$p_out
estimates$dprofit_net <- estimates$dprofit_gross - estimates$dtax

# Suavização dos resultados ----
smooth <- estimates %>%
  group_by(qw_scen, qr_scen, ad) %>%
  mutate(dprofit_net_smooth = lowess(p, dprofit_net, f = 2 / 36)$y)

# Inspeção da suavização ----
c(1, 2, 3) %>%
  map(
    ~
     smooth  %>%
      filter(qr_scen == .x & p_out < 0.05) %>%
      ggplot(aes(
        x = p,
        y = dprofit_net_smooth,
        color = ad,
        group = ad
      )) +
      geom_line() +
      geom_point(aes(y = dprofit_net), alpha = 0.5) +
      facet_grid(qr_scen ~ qw_scen)
  )

write.csv2(smooth, "./results/simulation.csv")
