source("src/hierarchy.R")
source("src/weigths.R")
source("src/results.R")
source("src/excel.R")

library(igraph)
library(ggplot2)
library(gridExtra)
library(grid)
library(emojifont)
library(openxlsx2)
library(yaml)

# Load the configuration file
config <- yaml.load_file(file.path("config", "config.yml"))

# Simulation parameters
instances <- config$simulation$instances

# Maximum number of levels
max_levels <- config$simulation$max_levels

# Maximum number of branches (children)
max_branches <- config$simulation$max_branches

# Number of alternatives to be generated for each branch of the hierarchy 5, 10, 15, 25
alternatives <- config$simulation$alternatives

# Distribution type 0: uniform (N-1 DoF) 1: uniform (N DoF) 2: normal 3: exponential
distribution <- config$distribution$type

# Filter to regenate the weights in case of non desired values
# NULL: no filter, [0, 1]: parametric filter, 2: weak filter, 3: strong filter

filter_condition <- config$filter$condition
filter_strategy <- config$filter$strategy

# Number of instances to compute the hit ratio
hit_ratio_instances <- config$hit_ratio_instances

# Variable to control the logging of the output
log <- FALSE

# Determine the sheet name based on the conditions
sheet_name <- get_sheet_name(distribution, filter_condition)

# Create a output directory for charts and the output excel file
charts_folder <- setup_results_directory("output", sheet_name)

# Create the output excel file
wb <- setup_results_excel_file("output", sheet_name)

# Create a dataframe to store the output of each scenario
results <- create_results_data_frame(max_levels, max_branches, alternatives, hit_ratio_instances)

# Filter can only be applied to uniform distribution
if (!distribution %in% c(0, 1)) {
  filter_condition <- NULL
}

# Generate the criteria array
criteria <- vector()
for (branch in 2:max_branches) {
  for (level in 2:max_levels) {
    criteria <- c(criteria, branch ^ level)
  }
}

sorted_criteria <- sort(criteria)

# Structures to store the different charts
boxplots <- list()
violin_charts <- list()

index <- 1
for (level in 2:max_levels) {
  for (branch in 2:max_branches) {

    # Create the corresponding objective hierarchy
    myTree <- init_hierarchy()

    myTree <- setup_hierarchy(myTree, root_node = "Node", level = 1, max_levels = level, max_branches = branch, config)
    # As nodes are created, their weights are randomly generated, which are stored for the calculation of the TRUE
    # weights, but the SR method it also applied to them from the associated ordinal information and these weights
    # are stored for the HSR method. Moreover, a weight set is noy only generated, but as many as instances we want
    # to simulate. Therefore, a two weight vectors (TRUE and HSR) are stored in each node of the hierarchy.

    # Plot the created hierarchy
    coords <- layout_(myTree, as_tree())
    plot(myTree, layout = coords)

    criteriaIndex <- which(sorted_criteria == branch ^ level)

    for (alternativeIndex in seq(alternatives)) {

      alternative <- alternatives[alternativeIndex]

      print("-------------------------------------------------")
      print(paste("Level", level, "Branch", branch, "Alternatives", alternative))
      print("- - - - - - - - - - - - - - - - - - - - - - - - -")

      # Store the configuration in the output dataframe
      for (i in 1:(hit_ratio_instances)) {
        new_index <- hit_ratio_instances * (index - 1) + i

        if (distribution == 0) {
          results$distribution[new_index] <- "U(0,1) N-1 DoF"
        } else if (distribution == 1) {
          results$distribution[new_index] <- "U(0,1) N DoF"
        } else if (distribution == 2) {
          results$distribution[new_index] <- paste0("N(", config$distribution$mean, ",", config$distribution$sd, ")")
        } else if (distribution == 3) {
          results$distribution[new_index] <- paste0("exp(", config$distribution$lambda, ")")
        }

        results$level[new_index] <- level
        results$branch[new_index] <- branch
        results$alternative[new_index] <- alternative

        if (is.null(filter_condition)) {
          results$filter[new_index] <- "No filter"
        } else if (filter_condition == 2) {
          results$filter[new_index] <- "Weak"
        } else if (filter_condition == 3) {
          results$filter[new_index] <- "Strong"
        } else {
          results$filter[new_index] <- paste("Parametric:", filter_condition)
        }

        if (!is.null(filter_condition)) {
          if (filter_strategy == 0) {
            results$strategy[new_index] <- "Regenerate"
          } else if (filter_strategy == 1) {
            results$strategy[new_index] <- "Remove"
          }
        } else {
            results$strategy[new_index] <- "-"
        }
      }

      # Create a dataframe to store the output of the simulations
      simulations <- data.frame(
        hit_ratio_SR = numeric(length = instances * hit_ratio_instances),
        kendall_tau_SR = numeric(length = instances * hit_ratio_instances),
        hit_ratio_HSR = numeric(length = instances * hit_ratio_instances),
        kendall_tau_HSR = numeric(length = instances * hit_ratio_instances)
      )

      # Repeat the analyses for each instance
      for (instance in 1:instances) {
        print(paste("Instance", instance))

        # Compute the TRUE attribute weights and the corresponding TRUE ranking
        # (using the local TRUE weigths at the different levels and branches of the hierarchy)
        weights_TRUE <- get_attribute_weights_TRUE(myTree, "Node", instance)
        if (log) {
          print("weightsTRUE")
          print(weights_TRUE)
        }

        # Check if the vector is 0 which means that it is removed
        if (all(weights_TRUE == 0)) {
          for (i in 1:hit_ratio_instances) {
            new_index <- hit_ratio_instances * (instance - 1) + i
            simulations$hit_ratio_SR[new_index] <- NA
            simulations$hit_ratio_HSR[new_index] <- NA
          }
          simulations$kendall_tau_SR[new_index] <- NA
          simulations$kendall_tau_HSR[new_index] <- NA
          next
        }

        # Apply SR method over the TRUE attribute weights to derive the SR ranking
        # weightsSR
        weights_SR <- SR_method(weights_TRUE)
        if (log) {
          print("weightsSR")
          print(weights_SR)
        }

        # Compute the attribute weights derived from the SR method at the different levels and branches of the hierarchy, i.e.,
        # the HSR weights, and then the corresponding HSR ranking
        weights_HSR <- get_attribute_weights_HSR(myTree, "Node", instance)
        if (log) {
          print("weightsHSR")
          print(weights_HSR)
        }

        # Generate the utility matrix
        utility_matrix <- generate_utility_matrix(alternative, branch, weights_TRUE, weights_SR, weights_HSR)
        if (log) {
          print("utility_matrix")
          print(utility_matrix)
        }

        # Compute the ranking of the alternatives
        ranking_matrix <- get_ranking(utility_matrix)
        if (log) {
          print("matrix_ranking")
          print(ranking_matrix)
        }

        # Compute the hit ratio and Kendall's τ for SR and HSR methods in the current instance and save them
        kendall_tau_SR <- get_kendall_tau(ranking_matrix, "SR")
        kendall_tau_HSR <- get_kendall_tau(ranking_matrix, "HSR")

        for (i in 1:hit_ratio_instances) {
          new_index <- hit_ratio_instances * (instance - 1) + i

          # Compute the hit ratio for SR and HSR methods
          hit_ratio_SR <- get_hit_ratio(ranking_matrix, "SR", i)
          simulations$hit_ratio_SR[new_index] <- hit_ratio_SR
          if (log) {
            print("hit_ratio_SR")
            print(hit_ratio_SR)
          }

          hit_ratio_HSR <- get_hit_ratio(ranking_matrix, "HSR", i)
          simulations$hit_ratio_HSR[new_index] <- hit_ratio_HSR
          if (log) {
            print("hit_ratio_HSR")
            print(hit_ratio_HSR)
          }

          # Compute Kendall's τ for SR and HSR methods
          simulations$kendall_tau_SR[new_index] <- kendall_tau_SR
          if (log) {
            print("kendall_tau_SR")
            print(kendall_tau_SR)
          }

          simulations$kendall_tau_HSR[new_index] <- kendall_tau_HSR
          if (log) {
            print("kendall_tau_HSR")
            print(kendall_tau_HSR)
          }
        }
      }

      # Compute statistics for hit ratios and Kendall's τ for SR and HSR methods for the different instances of the objective hierarchy
      boxplots[[(alternativeIndex - 1) * length(alternatives) + criteriaIndex]] <- ggplot(data = simulations) +
        geom_boxplot(aes(y = kendall_tau_SR, x = "Kendall's τ SR", ymin = 0, ymax = 1)) +
        geom_boxplot(aes(y = kendall_tau_HSR, x = "Kendall's τ HSR", ymin = 0, ymax = 1)) +
        xlab("") + ylab("") +
        ylim(NA, 1) +
        theme(text = element_text(family = "serif"))

      violin_charts[[(alternativeIndex - 1) * length(alternatives) + criteriaIndex]] <- ggplot(data = simulations) +
        geom_violin(aes(y = kendall_tau_SR, x = "Kendall's τ SR", fill = "Kendall's τ SR"), alpha = 0.8) +
        geom_violin(aes(y = kendall_tau_HSR, x = "Kendall's τ HSR", fill = "Kendall's τ HSR"), alpha = 0.8) +
        xlab("") + ylab("") +
        theme(text = element_text(family = "serif"), legend.position = "none")

      chart_name <- paste("criteria", branch ^ level, "alternatives", alternative, sep = "_")
      chart_title <- paste("Num. of Criteria:", branch ^ level, ", Num. of Alternatives:", alternative)

      pdf(file = paste0(paste(charts_folder, "boxplots/", sep = "/"), chart_name, ".pdf"), width = 10, height = 10)
      plot(boxplots[[(alternativeIndex - 1) * length(alternatives) + criteriaIndex]]
             + xlab("Method")
      )
      dev.off()

      pdf(file = paste0(paste(charts_folder, "violin charts/", sep = "/"), chart_name, ".pdf"), width = 10, height = 10)
      plot(violin_charts[[(alternativeIndex - 1) * length(alternatives) + criteriaIndex]]
             + xlab("Method")
      )
      dev.off()

      # Store the output in the output dataframe
      results <- store_results_data_frame(results, index, simulations, hit_ratio_instances)

      index <- index + 1
    }
    print("-------------------------------------------------")
  }
}

# Create formatted row and column labels
row_labels <- lapply(alternatives,
                     function(alternative) textGrob(paste("Num. of Alternatives:", alternative),
                                                    rot = 90,
                                                    gp = gpar(fontfamily = "serif"),
                                                    just = "center"
                     )
)

col_labels <- lapply(criteria,
                     function(criteria) textGrob(paste("Num. of Criteria:", criteria),
                                                 gp = gpar(fontfamily = "serif"),
                                                 just = "center"
                     )
)

pdf(file = file.path(charts_folder, "all_boxplot.pdf"), width = 10, height = 10)
grid.arrange(arrangeGrob(
  grid.rect(gp = gpar(col = NA)),
  do.call(arrangeGrob, c(col_labels, ncol = length(criteria))),
  arrangeGrob(grobs = row_labels, ncol = 1),
  do.call(arrangeGrob, c(boxplots, ncol = length(criteria))),
  ncol = 2,
  widths = unit.c(unit(2, "lines"), unit(1, "null")),
  heights = unit.c(unit(2, "lines"), unit(1, "null"))
))
dev.off()

pdf(file = file.path(charts_folder, "all_violin_charts.pdf"), width = 10, height = 10)
grid.arrange(arrangeGrob(
  grid.rect(gp = gpar(col = NA)),
  do.call(arrangeGrob, c(col_labels, ncol = length(criteria))),
  arrangeGrob(grobs = row_labels, ncol = 1),
  do.call(arrangeGrob, c(violin_charts, ncol = length(criteria))),
  ncol = 2,
  widths = unit.c(unit(2, "lines"), unit(1, "null")),
  heights = unit.c(unit(2, "lines"), unit(1, "null"))
))
dev.off()

# Columns to exclude
if (distribution %in% c(0, 1) && !is.null(filter_condition)) {
  excluded_cols <- "simulations"
} else {
  excluded_cols <- c("simulations", "filter", "strategy")
}

# Write the output in the excel file
wb$add_data(sheet = sheet_name, x = results[, !(names(results) %in% excluded_cols)])

# Set column widths
wb$set_col_widths(sheet = sheet_name, cols = seq(1, length(results[, names(results) != "simulations"])), widths = "auto")

# Save the output in an excel file
save_excel(wb, dir_name = "output")
