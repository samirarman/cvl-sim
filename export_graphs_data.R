library(tidyverse)
library(Rcpp)
library(RcppDist)
sourceCpp("functions.cpp")

# Exemplo do efeito do MCM ------
revs <- seq(200e3, 400e3, 1e3)
tax_nomcm <- map_dbl(revs, ~tax(rep(.x, 13)))

set.seed(123)
mcm <- map(revs, ~replicate(1000, tax(rnorm(13, .x, 42359.15))))
tax_mcm <- map_dbl(mcm, ~mean(.x, na.rm = TRUE))
p_out <- map_dbl(mcm, ~sum(is.na(.x)/length(.x)))

data.frame(revs, no_mcm, tax_nomcm, p_out) %>%
  write_csv2("./results/mcm_effect.csv")

# Gráficos do comportamento
data <- read_csv2("./results/simulation.csv")

data %>%
  filter(qw_scen == 2 & qr_scen == 2 & ad == 4000) %>%
  select(p, ad, dprofit_net, dprofit_net_smooth) %>%
  write_csv2("results/smoothing.csv")

data %>%
  filter(qw_scen == 2 & qr_scen == 2) %>%
  select(p, ad, dprofit_net_smooth) %>%
  pivot_wider(names_from = ad, values_from = dprofit_net_smooth) %>%
  write_csv2("results/p_graph.csv")

data %>%
  filter(p %in% 184:186 & qw_scen == 2 & qr_scen == 2) %>%
  select(p, ad, dprofit_net_smooth) %>%
  pivot_wider(names_from = ad, values_from = dprofit_net_smooth) %>%
  write_csv2("results/ad_graph.csv")

data %>%
  filter(qw_scen == 2 & qr_scen == 2) %>%
  select(p, ad, dcost) %>%
  pivot_wider(names_from = ad, values_from = dcost) %>%
  write_csv2("results/cost_graph.csv")


