library(ggplot2)
library(igraph)
library(dplyr)

# Generate a scale free network
N <- 1e3
g <- erdos.renyi.game(N, 0.005, directed = FALSE)

# Assign the critical threshold to each node
V(g)$z_c <- degree(g)

# Assign the initial state to each node
V(g)$z <- 0


## Dynamics ##
f <- 1 / N

iter <- 1e6
pb <- txtProgressBar(min = 0, max = iter, style = 3)

start_time <- Sys.time()
data <- data.frame()
for (i in 1:iter){ # Shold be a stopping condition
  unstable_queue <- c()

  # Sample a node
  node <- sample(N, 1)

  # Update the state of the node
  V(g)[node]$z <- V(g)[node]$z + 1

  # Check if the node has become unstable
  if (V(g)[node]$z >= V(g)[node]$z_c){
    unstable_queue <- c(node)
  }

  # Solve unstable queue
  S <- c() # Avalanche size
  T <- 1 # Avalanche duration
  G <- 0 # Toppled grains
  while (length(unstable_queue) > 0) {
    # Pick the first node in the queue
    node <- unstable_queue[1]
    S <- c(S, node)
    
    # Remove the node from the queue
    unstable_queue <- unstable_queue[-1]

    # Get the neighbors of the node
    neighbors <- neighbors(g, node)
    
    # Sample a fraction of the neighbors
    to_keep <- runif(length(neighbors)) < 1 - f
    neighbors <- neighbors[to_keep]
    G <- G + length(neighbors)
    
    # Update the state of the neighbors
    V(g)[neighbors]$z <- V(g)[neighbors]$z + 1

    # Update the state of the node
    V(g)[node]$z <- 0

    # Check if the neighbors have become unstable
    unstable_neighbors <- neighbors[V(g)[neighbors]$z >= V(g)[neighbors]$z_c]
    if (length(unstable_neighbors) > 0) {
      # Add the unstable neighbors to the queue
      unstable_queue <- unique(c(unstable_queue, unstable_neighbors))

      # Update duration when new nodes are added to the queue
      T <- T + 1
    }

  } # End of while loop

  A <- unique(S)
  if (length(S) > 0){
    # Save the avalanche size
    data <- rbind(data, data.frame(S = length(S), A = length(A), T = T, G = G))
  }

  setTxtProgressBar(pb, i)

} # End of for loop

close(pb)

end_time <- Sys.time()
print(paste("Execution time:", end_time - start_time))

# Save the data
write.csv(data, "ER.csv", row.names = FALSE)
