library(tidyverse)


data <- read_csv2("./generated-data/inv_aggregate.csv")

plot(data$qty, type = "l", 
     ylab = "Conjuntos vendidos (un)",
     ylim = c(0, 600))

# Exclusão dos dados com falta de tecido no mercado e
# mês da pandemia
plot(data$qty[-c(21, 22, 28)], type = "l",
              ylab = "Conjuntos vendidos",
     ylim = c(0, 600))


# Quantidades vendidas em função do preço e do investimento
# em publicidade ------------------

# Criando um vetor de quantidades com a média
# de pedidos por unidade
source("./R/parameters.R")
qty_per_bid <- data$qty[-c(21, 22, 28)] / (BR_0 * BW_0)
hist(qty_per_bid)
plot(density(qty_per_bid))

write_csv2(as.data.frame(qty_per_bid), "./generated-data/qty_per_bid.csv")
