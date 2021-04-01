library(tidyverse)

# Leitura dos valores mensais do IPCA
infl_raw <-
  read_delim(
    "./raw-data/ipca.csv",
    skip = 1,
    n_max = 36,
    delim = ";",
    col_names = c("month_year", "variation")
  )

# Correção dos dados
infl <-
  infl_raw %>%
  mutate(
    month = str_sub(month_year, 1, 2) %>% as.double,
    year = str_sub(month_year, 4, 7) %>% as.double,
    multiplier = 1 + variation / 100,
    month_year = NULL
  ) %>%
  relocate(month, year)

# Criação do valor do deflator
defl <- numeric(nrow(infl))
for (i in 1:nrow(infl)) {
  defl[i] <- prod(infl$multiplier[i:nrow(infl)])
}

# Carregando os dados de movimentação de estoque
inventory <- 
  read_csv2(
  "./raw-data/inventory.csv",
  col_types = cols(
    ano = col_double(),
    mes = col_double(),
    cat_prod = col_factor(),
    vrsai = col_double(),
    qtdsai = col_double()
  )) 

# Agregação dos dados de movimentação de estoque
# e cálculo da quantidade mensal média de conjuntos
# e do preço médio
inv_agg <- inventory %>% 
  group_by(ano, mes) %>%
  summarise(qty = sum(qtdsai) / 2,
            rev = sum(vrsai),
            p = (rev / qty)) %>%
  ungroup() %>%
  mutate(
    p_defl = p * defl,
    defl = defl
  )

# Gravação dos dados agregados
write_csv2(inv_agg, "./generated-data/inv_aggregate.csv")
