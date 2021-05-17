library(parallel)
library(pbmcapply)
library(tidyverse)
library(Rcpp)
library(RcppDist)
sourceCpp("functions.cpp")

# Determinação do número de iterações ----

# Gera a tabela dos erros médios absolutos para
# a variável desejada
gen_mae <- function(estimate, sample_sizes) {
  iter <- 50
  m <- matrix(nrow = length(sample_sizes), ncol = iter)
  rownames(m) <- as.character(sample_sizes)

  for (i in 1:length(sample_sizes)) {
    m[i, ] <-
      replicate(
        iter,
        make_estimates(185, 2000, iter = sample_sizes[i])[[estimate]]
      )
  }
  apply(m, 1, function(x) {
    mean(abs(x - 0))
  })
}

set.seed(123)
gen_mae("dprofit_tax", seq(5e3, 30e3, 5e3))
gen_mae("dcost", seq(5e3, 30e3, 5e3))

# Simulação ----

iter <- 25e3

grid <-
  expand.grid(
    p = 150:300,
    ad = seq(2000, 10000, 1000),
    bws = 1:3,
    brs = 1:3
  )

cores <- detectCores()

RNGkind("L'Ecuyer-CMRG")
set.seed(123)
mc.reset.stream()

res <- pbmcmapply(
  make_estimates,
  grid$p,
  grid$ad,
  grid$bws,
  grid$brs,
  iter,
  mc.cores = cores,
  mc.set.seed = TRUE,
  ignore.interactive = TRUE
)

names <- dimnames(res)[[1]]
r <- as.data.frame(matrix(unlist(t(res)), nrow = nrow(grid)))
colnames(r) <- names

# Suavização dos resultados -----
r_soft <- r %>%
  group_by(ad, qw_scen, qr_scen) %>%
  mutate(across(
    .cols = c(dcost, dprofit, dprofit_tax), #p_out doesn't need softening
    .fns = ~ lowess(p, .x, f = 2 / 36)$y
  ))

# Checagem dos resultados ----
head(r_soft)

r_soft %>%
  #filter(qw_scen == 1 & qr_scen == 1) %>%
  ggplot(aes(
    x = p,
    y = dprofit_tax,
    color = ad,
    group = ad
  )) +
  geom_line() +
  geom_line(aes(y = dprofit), linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 185, color = "red") +
  facet_grid(qr_scen ~ qw_scen)


r_soft %>%
  filter(qw_scen == 1 & qr_scen == 1) %>%
  ggplot(aes(
    x = p,
    y = 100 * p_out,
    color = ad,
    group = ad
  )) +
  geom_line() +
  geom_hline(yintercept = 5, col = "red")

write.csv2(r_soft, "./results/simulation.csv")