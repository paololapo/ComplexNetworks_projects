library(ggplot2)
library(igraph)
library(dplyr)
library(foreach)
library(doParallel)
source("simpleDynamics.R")

### Parameters ###
num_cores <- detectCores() # Get the number of cores
#num_cores <- 3
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


### Simulations (parallel computation) ###
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