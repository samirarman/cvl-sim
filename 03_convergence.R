source("./R/functions.R")
set.seed(123)

qty_per_bid <- read_csv2("./generated-data/qty_per_bid.csv")$x

p <- c(185)
ad <- c(2000)

iter <- c(10,50, 100, 500, 1000)
size <- c(100, 500, 1000)
reps <- 50

get_prob <- function(reps, p, ad, qty_per_bid, iter, size) {
  prob <- numeric(reps)
  sample <- numeric(reps)
  
  start <- Sys.time()
  for (i in 1:reps) {
    prob[i] <- compare_prob(p, ad, qty_per_bid, iter, size)$p_delta_profit
    sample[i] <- i
  }
  end <- Sys.time()
  
  mean_time = as.numeric(difftime(end, start, units = "secs")) / reps
  ttc = as.numeric(difftime(end, start, units = "mins")) / reps  * 1359
  error = prob - mean(prob)
  tibble(p, ad, iter, size, sample, prob, mean_time, ttc, error) 
}

print("Starting sampling...")
start <- Sys.time()
convergence <- 
  expand_grid(iter, size) %>% 
  pmap_dfr(~get_prob(reps, p, ad, qty_per_bid, .x, .y))
print(paste0("Sampling finished.", Sys.time() - start))
      
print("Writing results...")      
write_csv2(convergence, "./generated-data/convergence.csv")
print("Done!")
