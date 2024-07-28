library(ggplot2)
library(igraph)
library(dplyr)

### Some functions  ###

# Initialize the node features given the graph, F, q, and mode
initialize_features <- function(g, F, q, mode) {
  # Generate the features
  if (mode == "pois") {
    features <- lapply(1:vcount(g), function(x) rpois(F, q))
  } else if (mode == "unif") {
    features <- lapply(1:vcount(g), function(x) sample(1:q, F, replace = TRUE))
  }
  
  # Attach the feature to each node
  V(g)$features <- features

  return(g)
}


# Compute the cultural similarity (feature overlap) between two nodes
cultural_similarity <- function(g, i, j) {
  sum(V(g)[[i]]$features == V(g)[[j]]$features)
}


# Get the indices of the different features between two nodes
cultural_differences <- function(g, i, j) {
  which(V(g)[[i]]$features != V(g)[[j]]$features)
}


# Compute the cultural similarity between all pairs of nodes
edge_similarities <- function(g) {
  sapply(E(g), function(x) cultural_similarity(g, ends(g, x)[1], ends(g, x)[2]))
}


# Single step of the dynamical model
single_step <- function(g) {
  # Randomly select an edge
  random_edge <- sample(E(g), 1)

  # Get the nodes
  node_i <- ends(g, random_edge)[1]
  node_j <- ends(g, random_edge)[2]

  # Compute the cultural similarity
  cs <- cultural_similarity(g, node_i, node_j)

  # Compute the probability of interaction
  p <- cs / F

  # Interaction
  if (runif(1) < p && p < 1) {
    diff <- cultural_differences(g, node_i, node_j)
    feature_to_change <- sample(diff, 1)
    V(g)[[node_j]]$features[feature_to_change] <- V(g)[[node_i]]$features[feature_to_change]
  }

  return(g)
}


# Find the cultural domains
get_cultural_domains <- function(g) {
  # Get all unique feature sets
  unique_features <- unique(V(g)$features)

  sizes <- c()

  # Iterate over all unique feature sets
  for (f in unique_features) {
    # Get the nodes with the feature set
    nodes <- sapply(V(g)$features, function(x) identical(x, f))

    # Get the subgraph with the nodes
    subgraph <- induced_subgraph(g, nodes)

    # Find connected components in the subgraph and the LCC
    components <- components(subgraph)
  
    # Get the size of the LCC and update s_max
    sizes <- c(sizes, components$csize)
  }

  return(sizes)
}


# Find the largest cultural domain
get_s_max <- function(g) {
  sizes <- get_cultural_domains(g)
  return(max(sizes))
}


# Find the number of active bonds
get_n_active_bonds <- function(g, F) {
  es <- edge_similarities(g)
  active_bounds <- es[(es < F) & (es > 0)]
  return(length(active_bounds))
}



### Main simulation function ###

# Iterations, fixed L, F, varying q
L_list <- c(50, 100, 150)
F_list <- c(2, 3, 4, 7, 10)
q_list <- c(1, 100, 200, 230, 250, 300, 400, 500, 1000)
rep_mc <- 10
max_iterations <- 1e6
mode <- "pois"

log_scale <- unique(round(10^(seq(log(1), log(max_iterations), length.out = 48))))

res <- data.frame()

# Define the progress bar
i_pb <- 0
max_pb <- length(L_list) * length(F_list) * length(q_list) * rep_mc
pb <- txtProgressBar(min = 0, max = max_pb, style = 3)

# Loop over the different values of L
for (L in L_list){

  # Loop over the different values of L
  for (F in F_list){

    # Loop over the different values of q
    for (q in q_list){

      # Repeat the simulation 10 times
      for (mc in 1:rep_mc){

        # Create and initialize the graph
        g <- make_lattice(length = L, dim = 2)
        n_edges <- ecount(g)
        N <- vcount(g)
        g <- initialize_features(g, F, q, mode)

        # Iterate the dynamics
        for (j in 1:max_iterations){
          g <- single_step(g)

          # Save the results
          if (j %in% log_scale) {
            s_max <- get_s_max(g)
            n_active_bonds <- get_n_active_bonds(g, F)
            n_den <- n_active_bonds / n_edges

            res <- rbind(res,
              data.frame(mc = mc,
                         L = L,
                         F = F,
                         q = q,
                         s_max_den = s_max / N,
                         n_density = n_den,
                         iteration = j))
          }
        } # End of iterations

        i_pb <- i_pb + 1
        setTxtProgressBar(pb, i_pb)

      } # End of rep_mc
    } # End of q_list
  } # End of F_list
} # End of L_list

close(pb)