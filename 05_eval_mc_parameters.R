source("./R/mc_header.R")


conv_time <-
  expand_grid(size = c(1, 10, 50, 100), iter = c(10, 50, 100, 500)) %>%
  pmap_dfr( ~ convergence_time(size = ..1, iter = ..2, FALSE, method = "partial")$summary) 

plot(var_p_delta_margin ~ delta_time, data = conv_time)
text(
  var_p_delta_margin ~ delta_time,
  label = paste0(size, "/", iter),
  cex = 0.8,
  pos = 3,
  data = conv_time
)

plot(
  var_p_delta_margin ~ delta_time,
  data = conv_time,
  ylim = c(0, 0.001),
  xlim = c(0, 2)
)
text(
  var_p_delta_margin ~ delta_time,
  label = paste0(size, "/", iter),
  cex = 0.8,
  pos = 3,
  data = conv_time
)

chosen <- convergence_time(size = 10, iter = 100, FALSE, method = "partial")

p_delta_margin_conv <- cummean(chosen$p_delta_margin_series)
p_delta_cost_conv <- cummean(chosen$p_delta_cost_series)

plot(p_delta_margin_conv)
plot(p_delta_cost_conv)

tibble(iter = 1:100, p_delta_margin_conv, p_delta_cost_conv) %>% 
  write_csv2("./generated-data/convergence.csv")
