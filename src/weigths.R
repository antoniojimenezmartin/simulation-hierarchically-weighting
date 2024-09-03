# Generate the vector of weights for the children of a node using a given distribution
generate_weights <- function(branches, distribution, filter) {
  if (distribution$type == 0 || distribution$type == 1) {
    generate_uniform_weights(distribution$type, branches, filter)
  } else if (distribution$type == 2) {
    generate_normal_weights(branches, distribution$mean, distribution$sd)
  } else if (distribution$type == 3) {
    generate_exponential_weights(branches, distribution$lambda)
  } else {
    stop("Invalid distribution type")
  }
}

# Check if the weights satisfy the filter condition
check_filter_condition <- function(weights, filter_condition) {
  if (is.null(filter_condition)) {
    TRUE
  } else if (filter_condition >= 0 && filter_condition <= 1) {
    sorted_weights <- sort(weights, decreasing = TRUE)
    sorted_weights[1] - sorted_weights[2] <= filter_condition
  } else if (filter_condition == 2) {
    all(weights >= (0.05 / length(weights))) && all(weights <= (0.7 + (0.3 / length(weights))))
  } else if (filter_condition == 3) {
    all(weights >= (0.1 / length(weights))) && all(weights <= (0.6 + (0.25 / length(weights))))
  } else {
    stop("Invalid filter condition")
  }
}

# Generate the vector of weights using a uniform distribution and apply filters if necessary
generate_uniform_weights <- function(distribution, branches, filter) {
  repeat {
    if (distribution == 0) {
      # Uniform with N-1 degrees of freedom
      values <- runif(branches - 1, min = 0, max = 1)
      sorted_values <- c(1, sort(values, decreasing = TRUE))
      # Compute the pairwise differences
      weights <- vector()
      for (i in 1:(branches - 1)) {
        weights <- c(sorted_values[i] - sorted_values[i + 1], weights)
      }
      weights <- c(weights, sorted_values[branches])
    } else if (distribution == 1) {
      # Uniform with N degrees of freedom
      values <- runif(branches, min = 0, max = 1)
      sort_values <- sort(values, decreasing = TRUE)
      # Normalize the weights
      weights <- sort_values / sum(sort_values)
    }

    if (check_filter_condition(weights, filter$condition)) {
      break
    } else if (filter$strategy == 1) {
      weights <- rep(0, branches)
      break
    }
  }

  weights
}

# Generate the vector of weights using a normal distribution
generate_normal_weights <- function(branches, mean, sd) {
  weights <- NULL
  repeat {
    value <- rnorm(1, mean = mean, sd = sd)

    # Check if the value is in the interval [0, 2*mean]
    if (value >= 0 && value <= 2*mean) {
      weights <- c(weights, value)

      if (length(weights) == branches) {
        break
      }
    }
  }

  # Normalize the weights
  weights <- weights / sum(weights)

  weights
}

# Generate the vector of weights using an exponential distribution
generate_exponential_weights <- function(branches, lambda) {
  weights <- rexp(branches, rate = lambda)

  # Normalize the weights
  weights <- weights / sum(weights)

  weights
}

# SR method applied to vector of weights
SR_method <- function(weights) {
  n <- length(weights)
  # Create a data frame with the original indices and values
  df <- data.frame(index = seq_along(weights), value = weights)

  # Sort the data frame by the 'value' column
  df <- df[order(df$value, decreasing = TRUE),]

  divisor <- sum(1/(1:n)) + sum((n+1-(1:n))/n)

  # Compute the SR weights
  for (i in 1:n) {
    df$SR[i] <- (1/i + (n - i + 1) / n) / divisor
  }

  # Sort the data frame by the 'index' column
  df <- df[order(df$index), ]

  # Return the SR weights
  df$SR
}
