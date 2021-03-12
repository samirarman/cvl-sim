library(tidyverse)

infl_raw <-
  read_delim(
    "./raw-data/ipca.csv",
    skip = 1,
    n_max = 36 ,
    delim = ";",
    col_names = c("month_year", "variation")
  )

infl <-
  infl_raw %>%
  mutate(
    month = str_sub(month_year, 1, 2) %>% as.double,
    year = str_sub(month_year, 4, 7) %>% as.double,
    multiplier = 1 + variation / 100,
    month_year = NULL
  ) %>%
  relocate(month, year)

defl <- numeric(nrow(infl))

for (i in 1:nrow(infl)) {
  defl[i] <- prod(infl$multiplier[i:nrow(infl)])
}

infl$defl <- defl


# Carregando os dados de movimentação de estoque
inv <- read_csv2(
  "./raw-data/inventory.csv",
  col_types = cols(
    ano = col_double(),
    mes = col_double(),
    cat_prod = col_factor(),
    vrsai = col_double(),
    qtdsai = col_double(),
    prven = col_double()
  )
)

# Agregação dos dadaos de movimentação de estoque
inv_agg <-
  inv %>%
  group_by(ano, mes) %>%
  summarise(qty = sum(qtdsai) / 2,
            rev = sum(vrsai),
            p_mean = rev / qty) %>%
  ungroup() %>%
  mutate(
    rev = rev * defl,
    p_mean = p_mean * defl,
    margin = rev * 0.44,
    var_cost = rev * (1 - 0.44)
  )

write_csv2(inv_agg, "./generated-data/inv_aggregate.csv")

