# purpose: to automatically determine the appropriate fitness type
# binary -> binomial GLM
# continuous -> Gaussian GLM


detect_family <- function(y) {
  y_clean <- y[!is.na(y)]

  if (length(y_clean) == 0) {
    stop("No non-NA values in fitness vector")
  }

  unique_vals <- sort(unique(y_clean))

  # detect binary
  is_binary <- length(unique_vals) <= 2 &&
    all(unique_vals %in% c(0, 1))

  if (is_binary) {
    if (length(y_clean) < 10) {
      warning("Binary fitness detected, but sample size < 10 (may be unstable)")
    }

    if (all(y_clean == 0) || all(y_clean == 1)) {
      warning("Complete separation detected")
    }

    return(list(
      type = "binary",
      family = binomial("logit")
    ))
  } else {
    return(list(
      type = "continuous",
      family = gaussian()
    ))
  }
}
