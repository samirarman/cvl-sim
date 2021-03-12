library(tidyverse)
source("./R/functions.R")

set.seed(123)

agg_data <- read_csv2("./generated-data/inv_aggregate_corr.csv")

# Criando um vetor de quantidades com randomização total
qty_sold <- agg_data$qty

qty_sold_samples <- sample(agg_data$qty, 1000, replace = T)
bw_vector <-
  bw(rep(P_0, 1000), "avg") %>% add_noise(BW_SD) %>% correct_bw_prob()
br_vector <-
  br(rep(AD_0, 1000), "avg") %>% add_noise(BW_SD) %>% correct_bids_req()
qty_per_bid <- qty_sold_samples / (bw_vector * br_vector)

write_csv2(as_tibble(qty_per_bid), "./generated-data/qty_per_bid.csv")
