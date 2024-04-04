# Define a function to create a cache object
make_cache <- function() {
  cache <- NULL
  set_cache <- function(key, value) {
    cache[[key]] <- value
  }
  get_cache <- function(key) {
    if (is.null(cache)) {
      return(NULL)
    } else {
      return(cache[[key]])
    }
  }
  list(set_cache = set_cache, get_cache = get_cache)
}

# Define your main function with caching mechanism
my_mean <- function(x, cache_obj = NULL) {
  if (is.null(cache_obj)) {
    cache_obj <- make_cache()
  }
  
  cached_result <- cache_obj$get_cache("mean")
  if (!is.null(cached_result)) {
    cat("Using cached result.\n")
    return(cached_result)
  } else {
    cat("Computing mean...\n")
    result <- mean(x)
    cache_obj$set_cache("mean", result)
    return(result)
  }
}

# Example usage
data <- c(1, 2, 3, 4, 5)
cat("First call:\n")
print(my_mean(data))
cat("\nSecond call:\n")
print(my_mean(data))
