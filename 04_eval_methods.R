source("./R/mc_header.R")


base_case <-
  gen_series(P_0, AD_0, "avg", "avg", 1000, qty_per_bid, "partial")

base_margin <- base_case * (P_0 - VAR_COST)

par(mfrow = c(1, 3),
    pty = "m",
    cex = 1.1)
plot(
  density(base_margin),
  xlim = c(0, max(max(base_margin), max(agg_data$margin))),
  ylim = c(0, max(max(
    density(base_margin)$y
  ), max(
    density(agg_data$margin)$y
  ))),
  main = "",
  xlab = "Margem de contribuição (R$)",
  ylab = "Densidade de probabilidade"
)
lines(density(agg_data$margin), col = "blue")

plot(
  density(base_case),
  xlim = c(0, max(max(base_case), max(qty_sold))),
  ylim = c(0, max(max(
    density(base_case)$y
  ), max(
    density(qty_sold)$y
  ))),
  main = "",
  xlab = "Qtd. vendida (un)",
  ylab = ""
)
lines(density(qty_sold), col = "blue")


plot(density(qty_per_bid),
     xlab = "Qtd. média por pedido (un)",
     ylab = "",
     main = "")

par(mfrow = c(1, 1), pty = "m")

tibble(
  density(base_margin)$x,
  density(base_margin)$y,
  density(agg_data$margin)$x,
  density(agg_data$margin)$y,
  density(base_case)$x,
  density(base_case)$y,
  density(qty_sold)$x,
  density(qty_sold)$y,
  density(qty_per_bid)$x,
  density(qty_per_bid)$y
) %>% 
  write_csv2("./generated-data/eval_sim.csv")
