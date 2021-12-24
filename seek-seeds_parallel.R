library(parallel)

seek_seed <- function(seed, size, message) {
  set.seed(seed)
  text <- sample(letters, size = size, replace = TRUE) |>
    paste(collapse = "")
  ifelse(text == message, TRUE, FALSE)
}

iterate_seeds <- function(cl, .seeds, .message) {
  size <- nchar(.message)
  clusterApply(cl, .seeds, function(x) {
    # print(paste("Seed:", x))
    res <- seek_seed(x, size = size, message = .message)
    if (res) stop(paste("Success: Seed:", x))
  })
  invisible(NULL)
}

test_seed <- function(seed, choices, message) {
  set.seed(seed)
  sample(choices, size = nchar(message), replace = TRUE) |>
    paste(collapse = "") |>
    stringr::str_to_title()
}

# cl <- makeCluster(parallelly::availableCores(omit = 1, logical = FALSE), outfile = "iterate_seeds_log.txt")
cl <- makeCluster(parallelly::availableCores(omit = 1, logical = FALSE))
clusterExport(cl, "seek_seed")

# A quick test
iterate_seeds(cl, 1:1e3, "ab")
test_seed(234, letters, "xx")

# A real test

# iterate_seeds(cl, 0:1e5, "merry")
# iterate_seeds(cl, 0:1e6, "xmas")
test_seed(64927, letters, "xxxx")

# iterate_seeds(cl, 0e6:1e6, "merry")
# iterate_seeds(cl, 1e6:2e6, "merry")
# iterate_seeds(cl, 2e6:3e6, "merry")
iterate_seeds(cl, 3e6:5e6, "merry")
stopCluster(cl)


