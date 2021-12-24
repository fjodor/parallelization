#### Setup and Test ####

message = "A"
size = nchar(message)

library(progressr)

seek_seed <- function(seed, choices) {
  # if (length(seed) > 1) {
  #   stop("This function is not vectorized. Call with a single value for seed.
  #        Use lapply, map, etc. to iterate over seeds.")
  # }
  # p <- progressor(steps = length(seed))
  set.seed(seed)
  text <- sample(choices, size = size, replace = TRUE) |> stringr::str_to_title()
  if (text == message) {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    message(paste("Success! Seed:", seed))
    stop()
  }
  # p(paste("Seed: ", seed, "   Text:", text, "\n"))
  # p()
}

# Check if error is thrown
# Enable error handling in seed_seed()
seek_seed(1:10, choices = letters)

# Without progress bar
purrr::map(1:1e4, seek_seed, choices = letters)
lapply(0:1e4, seek_seed, choices = letters)

# Implementing a progress bar
handlers(global = TRUE)

iterate_seeds <- function(seeds = seeds) {
  lapply(seeds, function(x) {
    p <- progressor(steps = length(seeds))
    seek_seed(...)
    p()
  })
}

seeds <- 0:1e4
choices <- letters
message = "A"
iterate_seeds()

set.seed(16)
sample(letters, size = size, replace = TRUE) |> stringr::str_to_title()



#### Real application ####

seek_seed <- function(seed, choices, size, message) {
  set.seed(seed)
  text <- sample(choices, size = size, replace = TRUE) |>
    paste(collapse = "") |>
    stringr::str_to_title()
  if (text == message) {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    message(paste("Success! Seed:", seed))
    stop()
  }
}

iterate_seeds <- function(.seeds, .choices, .message) {
  size <- nchar(.message)
  p <- progressr::progressor(along = .seeds)
  lapply(.seeds, function(x) {
    seek_seed(x, choices = .choices, size = size, message = .message)
    p()
  })
  invisible(NULL)
}

test_seed <- function(seed, choices, message) {
  set.seed(seed)
  sample(choices, size = nchar(message), replace = TRUE) |>
    paste(collapse = "") |>
    stringr::str_to_title()
}

library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

iterate_seeds(0:100, letters, "A")
test_seed(16, letters, "x")

iterate_seeds(0:1e4, letters, "Ab")
test_seed(234, letters, "xx")

# The following is too slow this way ...
# iterate_seeds(0:1e6, letters, "Merry")


#### Parallelization ####
# Better not use, due to challenges with random numbers ...

library(future.apply)

iterate_seeds <- function(.seeds, .choices, .message) {
  size <- nchar(.message)
  p <- progressr::progressor(along = .seeds)
  future_lapply(.seeds, function(x) {
    seek_seed(x, choices = .choices, size = size, message = .message)
    p()
  })
  invisible(NULL)
}

# 
# plan(sequential)
# iterate_seeds(0:1e4, letters, "Merry")
# 
# library(parallel)
# cl <- makeCluster(parallelly::availableCores(omit = 1))
# makeCluster(cl)
# 
# plan(cluster)
# iterate_seeds(0:1e4, letters, "Merry")


#### Run calculations ####

library(progressr)
library(future.apply)

seek_seed <- function(seed, choices, size, message) {
  set.seed(seed)
  text <- sample(choices, size = size, replace = TRUE) |>
    paste(collapse = "")
  if (text == message) {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    message(paste("Success! Seed:", seed))
    stop()
  }
}

# iterate_seeds <- function(.seeds, .choices, .message) {
#   size <- nchar(.message)
#   p <- progressor(along = .seeds)
#   lapply(.seeds, function(x) {
#     seek_seed(x, choices = .choices, size = size, message = .message)
#     p()
#   })
#   invisible(NULL)
# }

iterate_seeds <- function(.seeds, .choices, .message) {
  size <- nchar(.message)
  p <- progressor(along = .seeds)
  future_lapply(.seeds, function(x) {
    seek_seed(x, choices = .choices, size = size, message = .message)
    p()
  }, future.seed = NULL)
  invisible(NULL)
}

test_seed <- function(seed, choices, message) {
  set.seed(seed)
  sample(choices, size = nchar(message), replace = TRUE) |>
    paste(collapse = "") |>
    stringr::str_to_title()
}

handlers(global = TRUE)
handlers("progress")

library(parallel)
cl <- makeCluster(parallelly::availableCores(omit = 1))
makeCluster(cl)

plan(cluster)

iterate_seeds(0:100, letters, "a")
test_seed(16, letters, "x")

iterate_seeds(0:1e5, letters, "merry")

stopCluster(cl)

bench::mark(
  par_hyper = {
    cl <- makeCluster(parallelly::availableCores(omit = 1), logical = TRUE)
    makeCluster(cl)
    plan(cluster)
    iterate_seeds(0:1e4, letters, "merry")
    stopCluster(cl)
  },
  par_physical = {
    cl <- makeCluster(parallelly::availableCores(omit = 1), logical = FALSE)
    makeCluster(cl)
    plan(cluster)
    iterate_seeds(0:1e4, letters, "merry")
    stopCluster(cl)
  },
  sequential = {
    plan(sequential)
    iterate_seeds(0:1e4, letters, "merry")
  }
)

stopCluster(cl)
