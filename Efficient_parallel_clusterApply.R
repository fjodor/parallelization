# Efficient R Programming

# Running R code in parallel

library(parallel)
library(bench)
library(tidyverse)

#### Regression analyses using simulated data ####

cl <- makeCluster(detectCores() - 1)
cl

set.seed(2020)
data <- data.frame(replicate(200, sample(0:100, 90, replace = TRUE)))
data$result <- sample(1:50, size = 90, replace = TRUE)

IVs <- names(data[-length(data)])

reg <- function(IV) {
  model <- as.formula(paste("result ~", IV))
  summary(lm(model, data = data))
}

reg("X1")
IV <- "X1"
reg(IV)

lapply(IVs, reg)

clusterApply(cl, IVs, reg)

clusterExport(cl, "data")

clusterApply(cl, IVs, reg)

times <- bench::mark(
  lapply(IVs, reg),
  map(IVs, reg),
  clusterApply(cl, IVs, reg)
)

times
plot(times)

