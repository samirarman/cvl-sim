library(tidyverse)
source("./R/functions.R")

agg_data <- read_csv2("./generated-data/inv_aggregate_corr.csv")
qty_per_bid <- agg_data$qty / (BR_0 * BW_0)

# Grid de procura -----
p <- 185:280
ad <- seq(2000, 10000, by = 1000)
bw_type <- c("avg", "ns", "sens")
br_type <- c("avg", "opt", "pes")

grid <- expand_grid(p, ad, bw_type, br_type)

grid$qty_mean <-  grid %>%
  pmap_dbl(
    ~ gen_series(
      p = ..1,
      ad = ..2,
      bw_type = ..3,
      br_type = ..4,
      size = 1,
      qty_vector = qty_per_bid,
      method = "estimated"
    )
  )

base_qty <- grid %>% 
  filter(p == P_0 & ad == AD_0) %>% 
  pull(qty_mean) %>% 
  mean

# Cálculos das variáveis de interesse no grid ----
grid <- grid %>%
  mutate(
    mean_revenue = qty_mean * p,
    mean_cost = qty_mean * VAR_COST,
    mean_margin = mean_revenue - mean_cost,
    base_revenue = base_qty * P_0,
    base_cost = base_qty * VAR_COST,
    base_margin = base_revenue - base_cost,
    mean_delta_margin = mean_margin - base_margin,
    mean_delta_cost = mean_cost - base_cost,
    investment = 12 * (ad - AD_0),
    ttr =  investment / mean_delta_margin
  )

write_csv2(grid, "./generated-data/expected_grid.csv")
