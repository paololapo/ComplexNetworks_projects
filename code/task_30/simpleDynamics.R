library(ggplot2)
library(igraph)
library(dplyr)
library(foreach)
library(doParallel)

### Functions  ###
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


plotState <- function(g, edge_cmap, iteration) {
  E(g)$similarity <- edge_similarities(g)
  edge_colors <- edge_cmap(F)[cut(E(g)$similarity, breaks=F)]

  plot(g, 
    layout=layout_on_grid(g),  # Use lattice layout for proper grid representation
    vertex.size=10,  # Size of the vertices
    vertex.label=NA,  # Font size of vertex labels
    edge.color=edge_colors,  # Color of the edges
    edge.width=5,  # Size of the edges
    vertex.color="lightblue",  # Color of the vertices
    vertex.frame.color="black",  # Border color of the vertices
    vertex.label.color="black",  # Color of the vertex labels
  )
  title(paste0("i: ", iteration), cex.main=2)
}


plotDynamics <- function(label, title, F_value, show=FALSE){
  # Load the data
  path <- paste0("../../data/task_30/", label, ".csv")
  res <- read.csv(path)

  # Prepare the data
  if (label == "resultsLattice") {
    df <- res |>
      filter (L == 50, F == F_value) |>
      group_by(q, iteration) |>
      summarize(
        s_avg=mean(s_max_den), s_sd=sd(s_max_den), 
        n_avg=mean(n_density), n_sd=sd(n_density),
        .groups = 'drop'
      )
  } else {
    df <- res |>
      filter (F == 2) |>
      group_by(q, iteration) |>
      summarize(
        s_avg=mean(s_max_den), s_sd=sd(s_max_den), 
        n_avg=mean(n_density), n_sd=sd(n_density),
        .groups = 'drop'
      )
  }

  # Plot active bonds
  p1 <- ggplot(df, aes(x = iteration, y = n_avg, color = factor(q))) +
    geom_point() +  # Line plot for the mean values
    geom_errorbar(aes(ymin = n_avg - n_sd, ymax = n_avg + n_sd), width = 0.05) +  # Error bars
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k=15), se = FALSE, linewidth = 0.6) + # Smooth line
    scale_x_log10() +  # Log scale for x-axis
    scale_y_log10() +  # Log scale for y-axis
    labs(
      title = title,
      x = "Iteration",
      y = "<n_active>/E",
      color = "q"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 25),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15)
    )
    
  if (show){
    suppressWarnings(print(p1))
  }

  # Plot largest cultural domain
  p2 <- ggplot(df, aes(x = iteration, y = s_avg, color = factor(q))) +
    geom_point() +  # Line plot for the mean values
    geom_errorbar(aes(ymin=(s_avg-s_sd), ymax=(s_avg+s_sd), width = 0.05)) +  # Error bars
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k=25), se = FALSE, linewidth = 0.6) +
    scale_x_log10() +  # Log scale for x-axis
    labs(
      title = title,
      x = "Iteration",
      y = "<S_max>/N",
      color = "q"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 25),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15)
    ) 
    
  if (show){
    suppressWarnings(print(p2))
  }

  return(list(p1, p2))
}

plotTransition <- function(label, title, show=FALSE){
  # Load the data
  path <- paste0("../../data/task_30/", label, ".csv")
  res <- read.csv(path)

  # Prepare the data
  df <- res |>
    filter(iteration == max(iteration)) |>
    group_by(q, F) |>
    summarize(
      s_avg=mean(s_max_den), s_sd=sd(s_max_den), 
      n_avg=mean(n_density), n_sd=sd(n_density),
      .groups = 'drop'
    )

  p <- ggplot(df, aes(x = q, y = s_avg, color = factor(F))) +
    geom_line() +  # Line plot for the mean values
    geom_point() +  # Line plot for the mean values
    geom_errorbar(aes(ymin=(s_avg-s_sd), ymax=(s_avg+s_sd), width = 0.05)) +  # Error bars
    #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k=1), se = FALSE, linewidth = 0.6) +
    # scale_x_log10() +  # Log scale for x-axis
    labs(
      title = title,
      x = "q",
      y = "<S_max>/N",
      color = "F"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    scale_fill_brewer(palette = "Set2")
    
  if (show){
    suppressWarnings(print(p))
  }

  return(p)
}


plotCulturalConvergence <- function(labels, labels2, q_values, F_value){
  preprocess_data <- function(df, label, F_value, q_values) {
    d <- df |>
      filter (F == F_value) |>
      filter (iteration == max(iteration)) |>
      filter (q %in% q_values) |>
      group_by(q) |>
      summarize(
        s_avg=mean(s_max_den), s_sd=sd(s_max_den),
        n_avg=mean(n_density), n_sd=sd(n_density),
        .groups = 'drop'
      ) |>
      mutate (Label = label)
    return(d)
  }

  # Load and preprocess the data
  file_paths <- lapply(labels, function(x) paste0("../../data/task_30/", x, ".csv"))
  data_list <- lapply(file_paths, function(x) read.csv(x))

  processed_data_list <- map2(data_list,
                              labels2,
                              ~preprocess_data(.x, .y, F_value = F_value, q_values = q_values))
                              
  final_data <- bind_rows(processed_data_list)
  final_data$q <- factor(final_data$q)

  # Create the plots
  p1 <-ggplot(final_data, aes(x = Label, y = s_avg, fill = q)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_errorbar(aes(ymin = s_avg - s_sd, ymax = s_avg + s_sd), 
                  position = position_dodge(0.7), width = 0.25) +  
    labs(title = "Size of largest cultural domain",
        x = "", y = "<s_max>/N") +
    scale_fill_brewer(palette = "Set2",
        guide = guide_legend(ncol = 4, direction="horizontal")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13)
          )
          
  p2 <- ggplot(final_data, aes(x = Label, y = n_avg, fill = q)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_errorbar(aes(ymin = n_avg - n_sd, ymax = n_avg + n_sd), 
                  position = position_dodge(0.7), width = 0.25) +  
    labs(title = "Density of active bonds",
        x = "", y = "<n_active>/E") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13)
          )
  
  # Extract the legend from one of the plots
  legend <- get_legend(p1)

  # Remove legends from the individual plots
  p1 <- p1 + theme(legend.position = "none")
  p2 <- p2 + theme(legend.position = "none")

  # Combine the plots with a shared legend below
  combined_plot <- plot_grid(p1, p2, ncol=2)  # Combine the plots
  final_plot <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.06))  # Add the legend at the bottom
  
  return(final_plot)
}