library(ggplot2)
library(igraph)
library(dplyr)
library(foreach)
library(doParallel)
#library(pryr)

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
# Parallel computation
num_cores <- detectCores() # Get the number of cores
num_cores <- 3
cl <- makeCluster(num_cores, outfile = "Log.txt") # Create a cluster with the number of cores
registerDoParallel(cl) # Register the parallel backend

# Iterations, fixed L, F, varying q
L_list <- c(30)
F_list <- c(2, 5, 10)
q_list <- c(1, 10, 100, 200, 300, 500)
rep_mc <- 1
max_iterations <- 1e8
mode <- "pois"
log_scale <- unique(round(10^(seq(log10(1), log10(max_iterations), length.out = 96))))


# Start timing the computation
start_time <- Sys.time()

# Loop over the different values of L and F
res <- foreach(L = L_list, .combine = rbind) %:%
  foreach(F = F_list, .combine = rbind) %:%
  foreach(q = q_list, .combine = rbind, .packages = c('igraph')) %dopar% {
    # Initialize a local result variable
    local_res <- data.frame()

    # Repeat the simulation 10 times
    for (mc in 1:rep_mc) {
      print(paste("Processing L:", L, "F:", F, "q:", q, "mc:", mc, " on worker", Sys.getpid()))

      # Create and initialize the graph
      g <- make_lattice(length = L, dim = 2)
      n_edges <- ecount(g)
      N <- vcount(g)
      g <- initialize_features(g, F, q, mode)

      # Iterate the dynamics
      for (j in 1:max_iterations) {
        g <- single_step(g)

#        if (j %% 5e5 == 0) {
#	  gc()
#	}

        # Save the results
        if (j %in% log_scale) {
      	  print(object.size(g))
	        print(paste("It: ", j))

          s_max <- get_s_max(g)
          n_active_bonds <- get_n_active_bonds(g, F)
          n_den <- n_active_bonds / n_edges

          local_res <- rbind(local_res,
            data.frame(mc = mc,
                       L = L,
                       F = F,
                       q = q,
                       s_max_den = s_max / N,
                       n_density = n_den,
                       iteration = j))
        }
      } # End of iterations

      rm(g)

      # Update iteration counter and log progress
    } # End of rep_mc
    
    print(paste("Results: s_max_den", s_max/N, "n_density", n_den))
    # write.csv(local_res, paste0("lres_", end_time, ".csv"), row.names = FALSE) ##

    return(local_res)
  } # End of nested foreach loops

# Stop the cluster
stopCluster(cl)

# End timing the computation
end_time <- Sys.time()
elapsed_time <- end_time - start_time
cat(sprintf("Total computation time: %s\n", elapsed_time))

# Save res in csv
write.csv(res, paste0("res_", end_time, ".csv"), row.names = FALSE)
