# Create output data frame
create_results_data_frame <- function(max_levels, max_branches, alternatives, hit_ratio_instances) {
  total_length <- (max_levels - 1) * (max_branches - 1) * length(alternatives) * hit_ratio_instances

  results <- data.frame(
    distribution = vector(length = total_length),
    level = numeric(length = total_length),
    branch = numeric(length = total_length),
    alternative = numeric(length = total_length),
    filter = numeric(length = total_length),
    strategy = numeric(length = total_length),
    simulations = I(vector("list", length = total_length)),
    hit_ratio_instances = rep(1:hit_ratio_instances, times = (max_levels - 1) * (max_branches - 1) * length(alternatives)),
    percentage_hit_SR = numeric(length = total_length),
    percentage_hit_HSR = numeric(length = total_length),
    min_kendall_tau_SR = numeric(length = total_length),
    max_kendall_tau_SR = numeric(length = total_length),
    mean_kendall_tau_SR = numeric(length = total_length),
    sd_kendall_tau_SR = numeric(length = total_length),
    min_kendall_tau_HSR = numeric(length = total_length),
    max_kendall_tau_HSR = numeric(length = total_length),
    mean_kendall_tau_HSR = numeric(length = total_length),
    sd_kendall_tau_HSR = numeric(length = total_length),
    q1_kendall_tau_SR = numeric(length = total_length),
    q3_kendall_tau_SR = numeric(length = total_length),
    q1_kendall_tau_HSR = numeric(length = total_length),
    q3_kendall_tau_HSR = numeric(length = total_length)
  )
}

# Store the output of the simulations in the output data frame
store_results_data_frame <- function(results, index, simulations, hit_ratio_instances) {
  # Compute the statistics of the simulations
  min_kendall_tau_SR <- min(simulations$kendall_tau_SR, na.rm = TRUE)
  max_kendall_tau_SR <- max(simulations$kendall_tau_SR, na.rm = TRUE)
  mean_kendall_tau_SR <- mean(simulations$kendall_tau_SR, na.rm = TRUE)
  sd_kendall_tau_SR <- sd(simulations$kendall_tau_SR, na.rm = TRUE)
  min_kendall_tau_HSR <- min(simulations$kendall_tau_HSR, na.rm = TRUE)
  max_kendall_tau_HSR <- max(simulations$kendall_tau_HSR, na.rm = TRUE)
  mean_kendall_tau_HSR <- mean(simulations$kendall_tau_HSR, na.rm = TRUE)
  sd_kendall_tau_HSR <- sd(simulations$kendall_tau_HSR, na.rm = TRUE)
  q1_kendall_tau_SR <- quantile(simulations$kendall_tau_SR, 0.25, na.rm = TRUE)
  q3_kendall_tau_SR <- quantile(simulations$kendall_tau_SR, 0.75, na.rm = TRUE)
  q1_kendall_tau_HSR <- quantile(simulations$kendall_tau_HSR, 0.25, na.rm = TRUE)
  q3_kendall_tau_HSR <- quantile(simulations$kendall_tau_HSR, 0.75, na.rm = TRUE)

  for (i in 1:hit_ratio_instances) {
    new_index <- hit_ratio_instances * (index - 1) + i

    # Store the simulations in the output dataframe
    results$simulations[[new_index]] <- simulations

    # Store the statistics in the output dataframe
    results$percentage_hit_SR[new_index] <- mean(simulations$hit_ratio_SR[seq(i, length(simulations$hit_ratio_SR), by = hit_ratio_instances)], na.rm = TRUE)
    results$percentage_hit_HSR[new_index] <- mean(simulations$hit_ratio_HSR[seq(i, length(simulations$hit_ratio_HSR), by = hit_ratio_instances)], na.rm = TRUE)

    results$min_kendall_tau_SR[new_index] <- min_kendall_tau_SR
    results$max_kendall_tau_SR[new_index] <- max_kendall_tau_SR
    results$mean_kendall_tau_SR[new_index] <- mean_kendall_tau_SR
    results$sd_kendall_tau_SR[new_index] <- sd_kendall_tau_SR
    results$min_kendall_tau_HSR[new_index] <- min_kendall_tau_HSR
    results$max_kendall_tau_HSR[new_index] <- max_kendall_tau_HSR
    results$mean_kendall_tau_HSR[new_index] <- mean_kendall_tau_HSR
    results$sd_kendall_tau_HSR[new_index] <- sd_kendall_tau_HSR
    results$q1_kendall_tau_SR[new_index] <- q1_kendall_tau_SR
    results$q3_kendall_tau_SR[new_index] <- q3_kendall_tau_SR
    results$q1_kendall_tau_HSR[new_index] <- q1_kendall_tau_HSR
    results$q3_kendall_tau_HSR[new_index] <- q3_kendall_tau_HSR
  }

  results
}

generate_utility_matrix <- function(alternatives, branches, weightsTRUE, weightsSR, weightsHSR) {
  # Create a matrix of utilities
  utility_matrix <- matrix(nrow = alternatives, ncol = 3)
  for (i in 1:alternatives) {
    # Generate a random set of weights for the alternatives
    alternative_weights <- runif(branches, min = 0, max = 1)
    # Compute the utility of each alternative
    utility_matrix[i, 1] <- sum(weightsTRUE * alternative_weights)
    utility_matrix[i, 2] <- sum(weightsSR * alternative_weights)
    utility_matrix[i, 3] <- sum(weightsHSR * alternative_weights)
  }
  utility_matrix
}

# From the utility matrix, compute the ranking of the alternatives
get_ranking <- function(utiliy_matrix) {
  ranking <- matrix(nrow = nrow(utiliy_matrix), ncol = 3)
  for (i in 1:3) {
    ranking[, i] <- rank(-utiliy_matrix[, i])
  }
  ranking
}

# Compute the Hit Ratio of the SR and HSR methods
get_hit_ratio <- function(ranking_matrix, method, k = 1) {
  true_values <- ranking_matrix[, 1]
  if (method == "SR") {
    method_values <- ranking_matrix[, 2]
  } else if (method == "HSR") {
    method_values <- ranking_matrix[, 3]
  } else {
    stop("Invalid method")
  }

  hit_ratio <- 1
  for (i in 1:k) {
    if (match(i, true_values) != match(i, method_values)) {
      hit_ratio <- 0
      break
    }
  }
  hit_ratio
}

# Compute the Kendall's τ of the SR and HSR methods
get_kendall_tau <- function(ranking_matrix, method) {
  true_values <- ranking_matrix[, 1]
  if (method == "SR") {
    method_values <- ranking_matrix[, 2]
  } else if (method == "HSR") {
    method_values <- ranking_matrix[, 3]
  } else {
    stop("Invalid method")
  }
  unname(cor.test(true_values, method_values, method = 'kendall')$estimate)
}
