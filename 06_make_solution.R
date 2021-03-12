source("./R/mc_header.R")

# Geração das probabilidades de delta_margin e delta_cost ----
p <- 185:280
ad <- seq(2000, 10000, by = 1000)
bw_type <- c("avg", "ns", "sens")
br_type <- c("avg", "opt", "pes")

grid <- expand_grid(p, ad, bw_type, br_type)

start <- Sys.time()
probs <- grid %>%
  pmap_dfr(
    ~ mc(
      p1  = ..1,
      ad1 = ..2,
      p2 = P_0,
      ad2 = AD_0,
      bw_type = ..3,
      br_type = ..4,
      qty_vector = qty_per_bid,
      size = 10,
      iter = 100,
      method = "full"
    )
  )
print(Sys.time() - start)



# Junção dos resultados ------
solution <- tibble(grid, probs)

write.csv2(solution, "./generated-data/probs_full.csv")
