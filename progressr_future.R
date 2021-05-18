# Progress Bar in R
# See https://cran.r-project.org/web/packages/progressr/vignettes/progressr-intro.html

library(progressr)

n <- 500

set.seed(2021)
biomarker <- data.frame(replicate(n, sample(0:100, n/2, replace = TRUE)))
biomarker$result <- sample(1:50, size = n/2, replace = TRUE)

IVs <- names(biomarker[-length(biomarker)])

# Function as in Efficient_parallel.R

biomarker_reg <- function(IV) {
  model <- as.formula(paste("result ~", IV))
  summary(lm(model, data = biomarker))
}

biomarker_reg("X1")

result <- lapply(IVs, biomarker_reg)
result[[n]]
broom::glance(result[[n]])
broom::tidy(result[[n]])

#### Prepare Function for Progress Bar ####
# lapply inside function: progress using p()

biomarker_reg <- function(IVs) {

  p <- progressr::progressor(along = IVs)
  # p <- progressor(steps = length(IVs))
  # p <- progressr::progressor(steps = length(IVs) / 10)
  
  lapply(IVs, function(x) {
    model <- as.formula(paste("result ~", x))
    Sys.sleep(0.001)
    p()    # Here you could add a custom message
    summary(lm(model, data = biomarker))
  })
}

#### Proceed as normal: Don't show progress bar ####

handlers(global = FALSE)   # reset in case code is re-run
rm(result)
result <- biomarker_reg(IVs)
broom::glance(result[[n]])

#### Show progress bar using with_progress() ####

handlers("txtprogressbar")  # the default, in case code is re-run

with_progress(
  result <- biomarker_reg(IVs)
)

#### Show progress bar using global handler ####

handlers(global = TRUE)
result <- biomarker_reg(IVs)

handlers("beepr")
# result <- biomarker_reg(IVs)



#### Use in parallel: future.apply ####

handlers(global = TRUE)
handlers("txtprogressbar")

library(future.apply)

biomarker_reg_p <- function(IVs) {
  
  p <- progressr::progressor(along = IVs)

  future_lapply(IVs, function(x) {
    model <- as.formula(paste("result ~", x))
    Sys.sleep(0.001)
    p()
    summary(lm(model, data = biomarker))
  })
}

plan(sequential)
rm(result)
result <- biomarker_reg_p(IVs)
broom::glance(result[[n]])

plan(multisession)
rm(result)
result <- biomarker_reg_p(IVs)
broom::glance(result[[n]])


library(parallel)
cl <- makeCluster(detectCores() - 1)

plan(cluster)
rm(result)
result <- biomarker_reg_p(IVs)
broom::glance(result[[n]])

handlers("progress")
result <- biomarker_reg_p(IVs)

stopCluster(cl)

