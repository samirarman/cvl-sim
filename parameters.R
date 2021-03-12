BW_EST <- c(0.25, 0.3)
BR_EST <- c(120, 150)
CONT_MARGIN <- 0.44

P_0 <- 185
AD_0 <- 2000
VAR_COST <- P_0 * (1 - CONT_MARGIN)

BW_0 <- mean(BW_EST)
BW_R <- -0.02
BW_MULT <- 0.6
BW_C <- 0
BW_D <- 1
BW_SCENARIO_PROB <- c(0.45, 0.3, 0.25)
BW_SD <- (BW_EST[2] - BW_EST[1])

BR_0 <- mean(BR_EST)
BR_R <- 3e-4
BR_MULT <- 0.6
BR_C <- 122
BR_D <- 250
BR_SCENARIO_PROB <- c(0.45, 0.3, 0.25)
BR_SD <- BR_EST[2] - BR_EST[1] 


