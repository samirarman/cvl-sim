library(Rcpp)
library(RcppDist)
sourceCpp("functions.cpp")

mae_table <- function(estimate, sample_sizes) {
    iter <- 50
    m <- matrix(nrow = length(sample_sizes), ncol = iter)
    rownames(m) <- as.character(sample_sizes)

    for (i in 1:length(sample_sizes)) {
        m[i, ] <-
            replicate(
                iter,
                dtax(0, iter = sample_sizes[i])[[estimate]]
            )
    }
    apply(m, 1, function(x) {
        mean(abs(x - 0))
    })
}

set.seed(123)
iters <- seq(5e3, 30e3, 5e3)
dtax_mae = as.data.frame(t(mae_table("dtax", iters)))
p_out_mae = as.data.frame(t(mae_table("p_out", iters)))

print(dtax_mae)
print(p_out_mae)

write.csv2(dtax_mae, "./results/dtax_mae.csv")
write.csv2(p_out_mae, "./results/p_out_mae.csv")
