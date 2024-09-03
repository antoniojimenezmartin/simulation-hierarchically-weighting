# Initialize the hierarchy with the Overall Objective node
init_hierarchy <- function() {
  tree <- make_graph(edges = NULL, n = 1, directed = TRUE)

  # General information common to intermediate and child nodes
  V(tree)$label[1] <- "Node"
  V(tree)$local_weight_TRUE[1] <- list(vector())
  V(tree)$local_weight_HSR[1] <- list(vector())
  V(tree)$num_children[1] <- 0

  tree
}

# Insert a new node in the hierarchy
insert_node <- function(tree, node_label, label_new) {
  # Check if the label (label_new) already exists in the tree
  if(!is.na(match(label_new, V(tree)$label))) {
    write("Error in function insert_node. This node label already exists", stderr())
    return(tree)
  }

  # Add the new node
  tree <- add_vertices(tree, 1)
  index <- length(V(tree))
  tree <- add_edges(tree, c(match(node_label, V(tree)$label), index))

  # Introduce the information of the new leaf
  V(tree)$label[index] <- label_new
  V(tree)$local_weight_TRUE[index] <- list(vector())
  V(tree)$local_weight_HSR[index] <- list(vector())
  V(tree)$num_children[index] <- 0

  # Increment the number of children of the father node
  V(tree)$num_children[match(node_label, V(tree)$label)] <- V(tree)$num_children[match(node_label, V(tree)$label)] + 1

  tree
}

# Store the TRUE weights generated from the children
save_children_weights_TRUE <- function(tree, node_label, weight_matrix) {
  direct_children <- ego(tree, order =1 , match(node_label, V(tree)$label), mode = "out")

  # Check if the node is a leaf
  if (V(tree)$num_children[[match(node_label, V(tree)$label)]] <= 0) {
    write("Error in function save_children_weights_TRUE: The node is a leaf, it is not possible to assign weights to its children", stderr())
    return(tree)
  }

  # Check if the number of children of this node coincides with the number of weights to assign
  if (length(direct_children[[1]]) - 1 != nrow(weight_matrix)) {
    write("Error in function save_children_weights_TRUE: The number of children of this node does not coincide with the number of weights to assign", stderr())
    return(tree)
  }

  # Assign the weights to the children
  for (child_index in 2:length(direct_children[[1]])) {
    V(tree)$local_weight_TRUE[[direct_children[[1]][child_index]]] <- weight_matrix[child_index - 1, ]
  }

  tree
}

# Save the HSR weights generated from the children
save_children_weights_HSR <-function(tree, node_label, weight_matrix){
  direct_children <- ego(tree, order =1 , match(node_label, V(tree)$label), mode = "out")

  # Check if the node is a leaf
  if (V(tree)$num_children[[match(node_label, V(tree)$label)]] <= 0) {
    write("Error in function save_children_weights_TRUE: The node is a leaf, it is not possible to assign weights to its children", stderr())
    return(tree)
  }

  # Check if the number of children of this node coincides with the number of weights to assign
  if (length(direct_children[[1]]) - 1 != nrow(weight_matrix)) {
    write("Error in function save_children_weights_TRUE: The number of children of this node does not coincide with the number of weights to assign", stderr())
    return(tree)
  }

  # Assign the weights to the children
  for (child_index in 2:length(direct_children[[1]])) {
    V(tree)$local_weight_HSR[[direct_children[[1]][child_index]]] <- weight_matrix[child_index - 1, ]
  }

  tree
}



# Get the labels of the descendents of a node
get_descendents <- function(tree, node_label) {
  names <- NULL

  # Check if the node exists
  if (is.na(match(node_label, V(tree)$label))) {
    write(paste0('Error in function get_descendents. There is no node with label "', node_label, '"'), stderr())
    return(names)
  }

  sons <- ego(tree, order = 10, match(node_label, V(tree)$label), mode = "out")[[1]]

  # Check if the node has descendents
  if (length(sons) < 2) {
      write("Warning in function get_descendents. The chosen node has no descendents", stderr())
      return(names)
  }

  # Get the labels of the descendents
  for (i in 2:length(sons)) {
      names <- c(names, V(tree)$label[sons[i]])
  }

  names
}

# Get the TRUE weights of the attributes over the decision
get_attribute_weights_TRUE <- function(tree, node_label, instance) {
  # Idenfity the descendents of the node node_label
  descendents <- get_descendents(tree, node_label)
  attribute_weights <- NULL

  # Analyze each leaf
  for (i in seq_along(tree)) {
    # Check if it is a leaf node and if it is a descendent of the node
    if (V(tree)$num_children[[i]] > 0 || is.na(match(V(tree)$label[[i]], descendents))) {
      next
    }

    # Get the weight of the attribute
    weight <- V(tree)$local_weight_TRUE[[i]][instance]

    # Get the parent node
    parent_node <- ego(tree, order = 1, nodes = i, mode = "in")[[1]][2]

    # Multiply by the weights in the path to the Overall objective
    while (V(tree)$label[parent_node] != node_label) {
      weight <- weight * V(tree)$local_weight_TRUE[[parent_node]][instance]

      parent_node <- ego(tree, order = 1, nodes = match(V(tree)$label[parent_node], V(tree)$label), mode = "in")[[1]][2]
    }
    attribute_weights <- c(attribute_weights, weight)
  }

  attribute_weights
}

# Get the HSR weights of the attributes over the decision
get_attribute_weights_HSR <- function(tree, node_label, instance) {
  # Idenfity the descendents of the node node_label
  descendents <- get_descendents(tree, node_label)
  attribute_weights <- NULL

  # Analyze each leaf
  for (i in seq_along(tree)) {
    # Check if it is a leaf node and if it is a descendent of the node
    if (V(tree)$num_children[[i]] > 0 || is.na(match(V(tree)$label[[i]], descendents))) {
      next
    }

    # Get the weight of the attribute
    weight <- V(tree)$local_weight_HSR[[i]][instance]

    # Get the parent node
    parent_node <- ego(tree, order = 1, nodes = i, mode = "in")[[1]][2]

    # Multiply by the weights in the path to the Overall objective
    while (V(tree)$label[parent_node] != node_label) {
      weight <- weight * V(tree)$local_weight_HSR[[parent_node]][instance]

      parent_node <- ego(tree, order = 1, nodes = match(V(tree)$label[parent_node], V(tree)$label), mode = "in")[[1]][2]
    }
    attribute_weights <- c(attribute_weights, weight)
  }

  attribute_weights
}

# Setup the hierarchy of objectives with a specific number of levels and branches (recursive function)
setup_hierarchy <- function(tree, root_node, level, max_levels, max_branches, config) {
  # Insert the children nodes
  for (branch in 1:max_branches) {
    son <- paste0(root_node, branch)
    tree <- insert_node(tree, root_node, son)
    if (level + 1 <= max_levels) {
      tree <- setup_hierarchy(tree, root_node = son, level + 1, max_levels, max_branches, config)
    }
  }

  # Assign the generated weights to the children
  TRUE_weigths_matrix <- matrix(nrow = max_branches, ncol = config$simulation$instances)
  HSR_weigths_matrix <- matrix(nrow = max_branches, ncol = config$simulation$instances)
  for (instance in seq(config$simulation$instances)) {
    vector_pesos_TRUE <- generate_weights(max_branches, config$distribution, config$filter)
    TRUE_weigths_matrix[, instance] <- vector_pesos_TRUE

    vector_pesos_HSR <- SR_method(vector_pesos_TRUE)
    HSR_weigths_matrix[, instance] <- vector_pesos_HSR
  }

  # Store the weights in the tree
  tree <- save_children_weights_TRUE(tree, root_node, TRUE_weigths_matrix)
  tree <- save_children_weights_HSR(tree, root_node, HSR_weigths_matrix)

  tree
}
