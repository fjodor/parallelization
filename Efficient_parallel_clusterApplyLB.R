# Efficient R Programming
# Running R code in parallel when runtimes of tasks differ

library(parallel)
library(snow)
library(bench)

set.seed(2020)
tasktime <- runif(30, min = 0, max = 0.1)
tasktime

cl <- makeCluster(detectCores() - 1)

bench::mark(
  lapply(tasktime, Sys.sleep),
  clusterApply(cl, tasktime, Sys.sleep),
  clusterApplyLB(cl, tasktime, Sys.sleep),
  iterations = 1
)

# It's snow time!

time_lapply <- snow.time(
  lapply(tasktime, Sys.sleep)
)

time_lapply
plot(time_lapply)

time_clusterApply <- snow.time(
  clusterApply(cl, tasktime, Sys.sleep)
)

time_clusterApply
plot(time_clusterApply)

time_clusterApplyLB <- snow.time(
  clusterApplyLB(cl, tasktime, Sys.sleep)
)

plot(time_clusterApplyLB)

stopCluster(cl)
