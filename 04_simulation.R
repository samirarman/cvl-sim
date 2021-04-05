source("R/functions.R")
set.seed(123)

# Carrega o conjunto q gerado previamente
qty_per_bid <- read_csv2("./generated-data/qty_per_bid.csv")$x

# Criação do grid
p <- 150:300
ad <- seq(2000, 10000, by = 1000)
grid <- expand_grid(p, ad)
iter <- 500
size <- 500

# Cria as quantidades esperadas para cada faixa de 
# preço e publiciade
qty <- grid %>%
  pmap_dbl( ~ qty_hat(p = .x, ad = .y, qty_per_bid = qty_per_bid))

estimates <- 
  tibble(grid, qty) %>%
  mutate(
    mean_revenue = qty * p,
    mean_cost_var = qty * (
      VAR_COST[1] * VAR_COST_PROB[1] + VAR_COST[2] * VAR_COST_PROB[2] + VAR_COST[3] * VAR_COST_PROB[3]
    ),
    mean_margin = mean_revenue - mean_cost_var,
    mean_delta_qty = qty - qty[p == P_0 & ad == AD_0],
    mean_delta_margin = mean_margin - mean_margin[p == P_0 &
                                                    ad == AD_0],
    mean_delta_cost_var = mean_cost_var - mean_cost_var[p == P_0 &
                                                          ad == AD_0],
    mean_delta_profit = mean_delta_margin - (ad - AD_0)
  ) %>%
  group_by(ad) %>%
  mutate(
    delta_price = p / lag(p, 1) - 1,
    delta_qty = qty / lag(qty, 1) - 1,
    elasticity = delta_qty / delta_price
  )

# Calcula as probabilidades
start <- Sys.time()
probs <-
  grid %>%
  pmap_dfr(~ compare_prob(.x, .y, qty_per_bid, iter, size))
print(Sys.time() - start)

# Salva as estimativas e probabilidades
solution <- tibble(estimates, probs)
write.csv2(solution, "./generated-data/solution.csv")
