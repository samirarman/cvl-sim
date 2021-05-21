library(tidyverse)

grid <- read_csv2("./results/simulation.csv") %>%
  filter(p_out < 0.05)

p0 <- 185
ad0 <- 2000

# Encontra o preço onde o lucro máximo ocorre
find_at_max <- function(data) {
  data %>%
    group_by(qr_scen) %>%
    summarise(
      max_dp = max(dprofit_net_smooth),
      p_at_point = p[which.max(dprofit_net_smooth)],
      ad_at_point = ad[which.max(dprofit_net_smooth)],
      dcost_at_point = dcost[which.max(dprofit_net_smooth)]
    ) %>%
    right_join(tibble(qr_scen = 1:3)) %>%
    t
}

# Encontra o preço onde o custo variável mínimo ocorre
find_at_min <- function(data) {
  data %>%
    group_by(qr_scen) %>%
    summarise(
      min_cost = min(dcost),
      p_at_point = p[which.min(dcost)],
      ad_at_point = ad[which.min(dcost)],
      dprofit_net_smooth = dprofit_net_smooth[which.min(dcost)]
    ) %>%
    right_join(tibble(qr_scen = 1:3)) %>%
    t
}

tables <- function(scenario) {
  # Encontra o ponto de lucro máximo
  max_profit <- grid %>%
    filter(qw_scen == scenario) %>%
    find_at_max

  # Encontra o investimento mais efetivo em
  # publicidade
  keep_price <- grid %>%
    filter(qw_scen == scenario & p == p0) %>%
    find_at_max

  # Encontra o maior valor de lucro quando se sobe
  # o preço, mantendo o capital
  opt_increase <-
    grid %>%
    filter(qw_scen == scenario &
             p > p0 & dprofit_net_smooth > 0 & dcost <= 0) %>%
    find_at_max

  # Encontra o menor capital requerido com lucro igual
  # ao atual
  min_cost <-
    grid %>%
    filter(qw_scen == scenario &
             p > p0 & dprofit_net_smooth > 0 & dcost <= 0) %>%
    find_at_min

  rbind(max_profit, keep_price, opt_increase, min_cost)
}

tables <- map(1:3, tables)
names(tables) <- c("Esperado", "Menos Sensível", "Mais sensível")

imap(tables, ~ write.csv2(.x,
                          paste0("./results/", .y, "-tax-table.csv")))
