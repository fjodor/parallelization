# Efficient R Programming
# Running for loops in parallel using foreach
# foreach package by Michelle Wallig and Steve Weston

#### Regression analyses using simulated data ####

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


#### Base R for loop ####

for (i in 1:length(IVs)) {
  IV <- IVs[i]
  print(reg(IV))
}

for_seq <- function(IVs) {
  models <- vector("list", length(IVs))
  for (i in seq_along(IVs)) {
    IV <- IVs[i]
    models[[i]] <- reg(IV)
  }
  models
}

for_seq(IVs)


#### foreach, sequentially ####

library(foreach)

foreach (n = IVs) %do%
  reg(n)

foreach_seq <- function(IVs) {
  foreach (n = IVs) %do%
    reg(n)
}

foreach_seq(IVs)


#### foreach, in parallel ####

library(parallel)
cl <- makeCluster(detectCores() - 1)
cl

library(doParallel)
registerDoParallel(cl)

foreach (n = IVs) %dopar%
  reg(n)

clusterExport(cl, c("reg", "data"))

foreach_par <- function(IVs) {
  foreach (n = IVs) %dopar%
    reg(n)
}

foreach_par(IVs)

library(bench)

times <- bench::mark(
  for_seq(IVs),
  foreach_seq(IVs),
  foreach_par(IVs),
  iterations = 5
)

times
ggplot2::autoplot(times)


#### Comparing for loops to parallel::clusterApply() ####

times <- bench::mark(
  lapply(IVs, reg),
  clusterApply(cl, IVs, reg),
  foreach_seq(IVs),
  foreach_par(IVs),
  iterations = 5
)

times
ggplot2::autoplot(times)

stopCluster(cl)
