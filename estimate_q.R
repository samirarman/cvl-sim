# Este arquivo contém a simulação da distribuição de
# probabilidade de q
library(truncnorm)
set.seed(123)

MEAN_Q = 520.5;
SD_Q = 115.61;

iter = 1e6
q = rtruncnorm(iter, 0, Inf, MEAN_Q, SD_Q) /
  (rtruncnorm(iter, 0, Inf, 135, 15) * rtruncnorm(iter, 0, Inf, .275, .025))

print(paste0("Média da distribuição de q: ", mean(q)))
print(paste0("Desvio padrão da distribuição de q: ", sd(q)))

h <- hist(q)

write.csv2(data.frame("mids" = h$mids, "counts" = h$counts),
           "./results/q_hist.csv")
