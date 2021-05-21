library(tidyverse)

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


