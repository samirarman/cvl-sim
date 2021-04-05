library(tidyverse)
library(extrafont)
library(gridExtra)
library(reshape)
source("./R/functions.R")

loadfonts()
# Configura o tema do ggplot2 para ficar o mais próximo
# do padrão PECEGE
theme_set(
  theme_minimal() + theme(
    title = element_text(
      family = "Arial",
      face = "plain",
      size = 11,
      color = "black"
    ),
    text = element_text(
      family = "Arial",
      face = "plain",
      size = 11,
      color = "black"
    ),
    axis.text = element_text(
      family = "Arial",
      face = "plain",
      size = 11,
      color = "black"
    ),
    legend.title = element_text(
      family = "Arial",
      face = "plain",
      size = 11,
      color = "black"
    ),
    legend.text = element_text(
      family = "Arial",
      face = "plain",
      size = 11,
      color = "black"
    ),
    legend.box.background =  element_rect(fill = "white", color = "black"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", size = 0.6),
    panel.grid = element_blank()
  )
)


# Distribuição de probabilidade de q ----

qty_per_bid <- read_csv2("./generated-data/qty_per_bid.csv")$x

tibble(qty_per_bid = qty_per_bid) %>%
  ggplot(aes(qty_per_bid)) +
  geom_density() +
  xlab("Conjuntos por pedido - R$") +
  ylab("Densidade de probabilidade")

ggsave(
  "./plots/qty_per_bid.png",
  device = "png",
  width = 12,
  height = 7,
  units = "cm"
)

# Gráficos de convergência ----
convergence <- read_csv2("./generated-data/convergence.csv")

p <- convergence %>%
  mutate(ttc = -1 * as.numeric(ttc)) %>%
  ggplot(aes(x = sample, y = 100 * error)) +
  geom_line() +
  geom_hline(yintercept = c(-0.5, .5), color = "lightgray") +
  geom_hline(yintercept = c(-0.25, 0.25), color = "darkgray") +
  facet_grid(size ~ iter)

ann <- convergence %>%
  group_by(size, iter) %>%
  summarise(label = max(as.numeric(ttc)))

p + geom_text(
  data = ann,
  mapping = aes(
    x = -Inf,
    y = -Inf,
    label = paste0("ttc=", round(label, 2), " min")
  ),
  hjust = -.1,
  vjust = -1
)

ggsave(
  "./plots/convergence.png",
  height = 16,
  width = 18,
  units = "cm"
)

# Comparativo de distribuições de probabilidade

r1 <- gen_br_samples(2000, size = 500) %>% density
r2 <- gen_br_samples(5000, size = 500) %>% density
a <- tibble(r1, type = "P = 184")
b <- tibble(r2, type = "P = 220")

tibble(r1_x = r1$x , r1_y = r1$y, r2_x = r2$x, r2_y = r2$y) %>% 
  write_csv2("./generated-data/r_compare.csv")
 
w1 <- (100 * gen_bw_samples(184, size = 500)) %>% density
w2 <- (100 * gen_bw_samples(220, size = 500)) %>% density

tibble(w1_x = w1$x, w1_y = w1$y, w2_x = w2$x, w2_y = w2$y) %>% 
  write_csv2("./generated-data/w_compare.csv")

# Solution plots ----

sim_results <- read_csv2("./generated-data/solution.csv")

sol_table <-
  sim_results %>%
  filter(mean_delta_profit > 0 &
           mean_delta_cost_var < 0) %>%
  group_by(ad) %>%
  summarise(
    min = min(p),
    max = max(p),
    max_p_delta_profit = max(p_delta_profit),
    max_delta_profit = mean_delta_profit[p_delta_profit == max(p_delta_profit)],
    p_delta_profit = p[p_delta_profit == max(p_delta_profit)],
    min_p_delta_cost = min(p_delta_cost),
    min_delta_profit = mean_delta_profit[p_delta_cost == min(p_delta_cost)],
    min_delta_cost_var = mean_delta_cost_var[p_delta_cost == min(p_delta_cost)],
    p_delta_cost = p[p_delta_cost == min(p_delta_cost)]
    
  )
write_csv2(sol_table, "./generated-data/sol_table.csv")

exploratory_table <-
  sim_results %>%
  filter(mean_delta_profit > 0) %>%
  group_by(ad) %>%
  summarise(
    min = min(p),
    max = max(p),
    max_p_delta_profit = max(p_delta_profit),
    max_delta_profit = mean_delta_profit[p_delta_profit == max(p_delta_profit)],
    p_at_max_delta_profit = p[p_delta_profit == max(p_delta_profit)],
    cost_at_max_delta_profit = mean_delta_cost_var[p_delta_profit == max(p_delta_profit)]
    
  )
write_csv2(exploratory_table, "./generated-data/exploratory_table.csv")

solution_plot <- function(var) {
  sim_results %>%
    ggplot(aes(x = p, y = ad, fill = {
      {
        var
      }
    })) +
    scale_y_continuous(breaks = seq(0, 10000, 1000)) +
    geom_raster() +
    scale_fill_viridis_c() +
    xlab("Preço - R$") +
    ylab("Investimento em \npublicidade - R$") +
    theme(panel.background =  element_blank())
}

p1 <- solution_plot(mean_delta_profit)
p2 <- solution_plot(p_delta_profit)
p3 <- solution_plot(mean_delta_margin)
p4 <- solution_plot(p_delta_margin)

g <- arrangeGrob(p1, p3, p2, p4, ncol = 2)
plot(g)
ggsave(
  "./plots/solution1.png",
  plot = g,
  width = 16,
  height = 18,
  units = "cm"
)

p1 <-
  solution_plot(mean_delta_cost_var)
p2 <-
  solution_plot(p_delta_cost)
p3 <-
  solution_plot(mean_delta_qty)
p4 <-
  solution_plot(p_delta_qty)
g <- arrangeGrob(p1, p3, p2, p4, ncol = 2)
plot(g)

ggsave(
  "./plots/solution2.png",
  plot = g,
  width = 16,
  height = 18,
  units = "cm"
)

sim_results %>%
  ggplot(aes(
    x = p,
    y = elasticity,
    groud = ad,
    color = ad
  )) +
  geom_line()

ggsave("elasticity.png",
       width = 12,
       height = 7,
       units = "cm")

sim_results %>%
  filter(mean_delta_margin > 0 & mean_delta_cost_var < 0) %>%
  ggplot(aes(x = p, y = ad, fill = mean_delta_margin)) +
  geom_tile() +
  scale_fill_viridis_c()
