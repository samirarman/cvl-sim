source("./R/functions.R")
set.seed(123)

inventory <- read_csv2("./generated-data/inv_aggregate.csv")

# Exclusão dos meses não representativos
inventory <- inventory[-c(21, 22, 28),]

# Sample com replacement
qty_sold_samples <- sample(inventory$qty, 5000, replace = T)

# Amostragem do cenário atual
bw_samples <-
  bw(rep(P_0, 5000), "avg") %>%
  add_noise(BW_SD) %>%
  correct_bw_prob()

br_samples <-
  br(rep(AD_0, 1000), "avg") %>%
  add_noise(BR_SD) %>%
  correct_bids_req()

# Geração do conjunto q
qty_per_bid <- qty_sold_samples / (bw_samples * br_samples)

# Salva os resultado para uso nos gráficos e na simulação
write.csv2(qty_per_bid, "./generated-data/qty_per_bid.csv")
