source("./R/functions.R")
set.seed(123)

qty_per_bid <- read_csv2("./generated-data/qty_per_bid.csv")$x

p <- c(150, 200, 250, 300)
ad <- c(2000, 10000)

iter <- tibble(iter = c(1000, 5000, 10000, 20000, 30000))

convergence <-
  expand_grid(p, ad, iter) %>%
  rowwise() %>%
  mutate(qty_hat = qty_hat(p, ad, qty_per_bid))

convergence$sim_res <-
  convergence %>% pmap_dbl(~ gen_series(..1, ..2, qty_per_bid, size = ..3) %>% mean)

convergence <-
  mutate(convergence, combination = paste0("P = ", p, " / AD = ", ad))

convergence %>%
  ggplot(aes(x = iter, color = combination, group = combination)) +
  geom_line(aes(y = sim_res)) +
  geom_line(aes(y = qty_hat), linetype = "dashed") +
  ylab("Conjuntos de uniformes - un.") +
  xlab("Iterações") +
  labs(color = "Combinações de \nP e AD")

ggsave(
  "./plots/convergence.png",
  height = 7,
  width = 12,
  units = "cm"
)
