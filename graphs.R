library(tidyverse)
library(gridExtra)


theme_set(theme_minimal() + theme(axis.title = element_text(face = "plain", size = 11)))

expected <- read_csv2("./generated-data/expected_grid.csv")
probs <- read_csv2("./generated-data/probs_partial.csv")

data <- left_join(expected, probs)

qty_per_bid <- read_csv2("./generated-data/qty_per_bid.csv")$value
# Plot delta margin ------


data %>% 
  filter(p_delta_margin > 0.5 & p_delta_cost > 0.5) %>% 
  ggplot(aes(x = ad, y = p, fill = mean_delta_margin)) +
  geom_raster() +
  xlab("Investimento mensal em publicidade por um ano (R$)") +
  labs(fill = "\u0394 margem") +
  ylab("Preço (R$)") +
  facet_grid(br_type ~ bw_type)
  ggsave("./plots/mean_margin_partial.png", device = "png", width = 13, height = 13, units = "cm", dpi = "print")

data %>% 
  filter(p_delta_margin > 0.5 & p_delta_cost > 0.5) %>% 
  ggplot(aes(x = ad, y = p, fill = p_delta_margin)) +
  geom_raster() +
  xlab("Investimento mensal em publicidade por um ano (R$)") +
  labs(fill = "P(\u0394 margem > 0)") +
  ylab("Preço (R$)") +
  facet_grid(br_type ~ bw_type) 
  ggsave("./plots/probs_margin_partial.png", device = "png", width = 13, height = 13, units = "cm", dpi = "print")

data %>% 
  filter(p_delta_margin > 0.5 & p_delta_cost > 0.5) %>% 
  ggplot(aes(x = ad, y = p, fill = p_delta_cost)) +
  geom_raster() +
  xlab("Investimento mensal em publicidade por um ano (R$)") +
  labs(fill = "P(\u0394 custo var > 0)") +
  ylab("Preço (R$)") +
  facet_grid(br_type ~ bw_type) 
  ggsave("./plots/probs_delta_cost_partial.png", device = "png", width = 13, height = 13, units = "cm", dpi = "print")

data %>% 
  filter(p != 185 & ad != 2000 & p_delta_margin > 0.5 & p_delta_cost > 0.5 & ttr <= 12 & ttr > 0) %>% 
  ggplot(aes(x = ad, y = p, fill = ttr)) + 
  geom_tile() +
  xlab("Investimento mensal em publicidade por um ano (R$)") +
  labs(fill = "Tempo de retorno") +
  ylab("Preço (R$)") +
  facet_grid(br_type ~ bw_type) 
  ggsave("./plots/ttr_partial.png", device = "png", width = 13, height = 13, units = "cm", dpi = "print")

var1 <- 
  data %>% 
  ggplot(aes(y = p_delta_margin, x = mean_delta_margin)) +
  geom_point() +
  ylab("P(\u0394M > 0)") +
  xlab("\u0394M") +
  ylim(0,1)

var2 <- 
  data %>% 
  ggplot() +
  geom_point(aes(y = p_delta_cost, x = mean_delta_cost)) +
  ylab("P(\u0394C > 0)") +
  xlab("\u0394C") +
  ylim(0,1)
  
p <- grid.arrange(var1, var2, ncol = 2)
  ggsave("./plots/probs_vs_estimation.png", plot = p, device = "png", width = 13, height = 7, units = "cm", dpi = "print")
  
  plot(density(qty_per_bid))
  