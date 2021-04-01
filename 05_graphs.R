library(tidyverse)
library(extrafont)
library(gridExtra)
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

# Curvas da função R ----
data <- tibble(
  ad = 2000:10000,
  'Esperado' = br(2000:10000),
  'Otimista' = br(2000:10000, "opt"),
  'Pessimista' = br(2000:10000, "pes")
) %>%
  pivot_longer(cols = -ad,
               names_to = "Cenário",
               values_to = "br")

data %>%
  ggplot(aes(
    x = ad,
    y = br,
    group = `Cenário`,
    color = `Cenário`
  )) +
  geom_line() +
  theme(legend.position = c(0.2, 0.8)) +
  scale_color_manual(values = c("black", "blue", "red")) +
  xlab("Investimento em publicidade - R$") +
  ylab("Pedidos de orçamentos recebidos")

ggsave(
  "./plots/r_curve.png",
  device = "png",
  width = 12,
  height = 7,
  units = "cm"
)


# Curvas da função W ----
data <- tibble(
  p = 150:300,
  'Esperado' = bw(150:300),
  'Mais senível' = bw(150:300, "ns"),
  'Menos sensível' = bw(150:300, "sens")
) %>%
  pivot_longer(cols = -p,
               names_to = "Cenário",
               values_to = "bw")

data %>%
  ggplot(aes(
    x = p,
    y = bw * 100,
    group = `Cenário`,
    color = `Cenário`
  )) +
  geom_line() +
  theme(legend.position = c(0.85, 0.8), ) +
  scale_color_manual(values = c("black", "blue", "red")) +
  xlab("Preço - R$") +
  ylab("Orçamentos vencidos - %")

ggsave(
  "./plots/w_curves.png",
  device = "png",
  width = 12,
  height = 7,
  units = "cm"
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

# Comparativos de densidade R e W ----


set.seed(123)
samples <- 2e4

# Função para produzir gráficos de densidade
dens_plot <- function(data, var) {
  data %>%
    ggplot(aes(x = x,
               linetype = {
                 {
                   var
                 }
               })) +
    geom_density() +
    theme(
      legend.text = element_text(hjust = 1),
      legend.position = c(0.85, 0.8),
      legend.box.just = "right",
      legend.box.background =  element_rect(fill = "white", color = "black")
    ) +
    ylab("Densidade de probabilidade")
}

# Comparativo entre as densidades de probabilidade
# de R com cenário médio quando AD = 2000 e AD = 10000
ad_2000 <- rnorm(samples, br(AD_0), BR_SD)
ad_10000 <- rnorm(samples, br(10000), BR_SD)

data <- tibble(x = c(ad_2000, ad_10000),
               AD = c(rep(2000, samples), rep(10000, samples)) %>% as_factor)

data %>%
  ggplot(aes(x = x,
             linetype = AD)) +
  geom_density() +
  theme(
    legend.text = element_text(hjust = 1),
    legend.position = c(0.85, 0.8),
    legend.box.just = "right",
    legend.box.background =  element_rect(fill = "white", color = "black")
  ) +
  ylab("Densidade de probabilidade") + 
  xlab("Número de pedidos recebidos")

ggsave(
  "./plots/r_density.png",
  device = "png",
  width = 12,
  height = 7,
  units = "cm"
)

# Comparativo entre as densidades de probabilidade
# de W com cenário médio quando P = 184 e P = 220
p_184 <- rnorm(samples, bw(P_0), BW_SD)
p_220 <- rnorm(samples, bw(220), BW_SD)

data <- tibble(x = c(p_184, p_220),
               P = c(rep(184, samples), rep(220, samples)) %>% as_factor)

data %>%
  ggplot(aes(x = 100 * x,
             linetype = P)) +
  geom_density() +
  theme(
    legend.text = element_text(hjust = 1),
    legend.position = c(0.85, 0.8),
    legend.box.just = "right",
    legend.box.background =  element_rect(fill = "white", color = "black")
  ) +
  ylab("Densidade de probabilidade") + 
  xlab("Orçamentos vencidos - %")

ggsave(
  "./plots/w_density.png",
  device = "png",
  width = 12,
  height = 7,
  units = "cm"
)

# Gráficos de convergência ----

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

# Comparação entre a distribuição das quantidades

analitical <- rnorm(10000, bw(300), BW_SD)
simulation <- rep(bw(300), 10000) %>% add_noise(BW_SD) %>% correct_bids_req()
tibble(analitical, simulation) %>% 
  ggplot()+
  geom_density(aes(analitical * 100)) +
  geom_density(aes(simulation * 100), linetype = "dashed") +
  geom_vline(xintercept = mean(analitical * 100)) +
  geom_vline(xintercept = mean(simulation * 100), linetype = "dashed") +
  xlab("Orçamentos vencidos - %") +
  ylab("Freqüência")

ggsave("./plots/qty_diff.png",
       height = 7,
       width = 12,
       units = "cm")

# Solution plots ----

sim_results <- read_csv2("./generated-data/solution.csv")

sol_table <- 
  sim_results %>%
  filter(mean_delta_profit > 0 &
           mean_delta_cost_var < 0) %>%
  group_by(ad) %>%
  summarise(min = min(p), 
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
  summarise(min = min(p), 
            max = max(p),
            max_p_delta_profit = max(p_delta_profit),
            max_delta_profit = mean_delta_profit[p_delta_profit == max(p_delta_profit)],
            p_at_max_delta_profit = p[p_delta_profit == max(p_delta_profit)],
            cost_at_max_delta_profit = mean_delta_cost_var[p_delta_profit == max(p_delta_profit)]
            
  )
write_csv2(exploratory_table, "./generated-data/exploratory_table.csv")

solution_plot <- function(var) {
  sim_results %>%
   filter(mean_delta_profit > 0) %>%
    ggplot(aes(x = p, y = ad, fill = {
      {
        var
      }
    })) +
    scale_y_continuous(breaks = seq(0, 10000, 1000)) +
    geom_raster() +
    scale_fill_viridis_c() +
    xlab("Preço - R$") +
    ylab("Investimento em \npublicidade - R$")
}


p1 <-
  solution_plot(mean_delta_profit) + labs(fill = "\u0394L", tag = "A")
p2 <-
  solution_plot(p_delta_profit) + labs(fill = "P(\u0394L > 0)", tag = "B")
p3 <-
  solution_plot(mean_delta_qty) + labs(fill = "\u0394Q", tag = "C")
p4 <-
  solution_plot(mean_delta_cost_var) + labs(fill = "\u0394C", tag = "D")
g <- arrangeGrob(p1, p2, p3, p4, nrow = 4)
ggsave(
  "./plots/solution.png",
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
       units = "cm"
       )
