library(terra)
library(geodata)
library(dplyr)
library(eurostat)
library(sf)

# Define some hyperparameters of the analysis
weight <- TRUE # If TRUE, edge list with the SCI as weights
dropNodeCode <- FALSE # If TRUE, drop the node code from the edge list


# =====================================================================
#                Load the data and prepare the tables
# =====================================================================

# Import the data from meta
path_data_fb <- "./data/task_44/meta/gadm1_nuts3_counties-gadm1_nuts3_counties - FB Social Connectedness Index - October 2021.tsv"
df <- read.table(file = path_data_fb, sep = "\t", header = TRUE)


# Filter out USA counties (for memory reasons)
if (any(grepl("USA", df$user_loc))) {
  df <- df[-grep("USA", df$user_loc),]
}
if (any(grepl("USA", df$fr_loc))) {
  df <- df[-grep("USA", df$fr_loc),]
}
cat("Filtered out USA counties, df has %d rows now", nrow(df))


# Import the table with the levels of the counties
levels <- read.table(file = "./data/task_44/meta/gadm1_nuts3_counties_levels.csv", sep = ',', header = TRUE)


# Attach the country to the levels table
levels$country <- sub("^([A-Z]+).*", "\\1", levels$key)

# Function to get the country from a NUTS3 CNTR_CODE
get_nuts3_country <- function(nuts3_code, nuts3_shapefile){
  nuts3_region <- nuts3_shapefile[nuts3_shapefile$id == nuts3_code, ]
  country <- countrycode::countrycode(nuts3_region$CNTR_CODE, origin = "eurostat", destination = "iso3c")
  return(country)
}

# Overwrite nuts3 countries with ISO 3 codes
nuts3_shapefile <- eurostat::get_eurostat_geospatial(nuts_level = 3, year = 2016, output_class = "sf")
nuts3_keys <- levels[levels$level == "nuts3", ]$key
nuts3_countries <- sapply(nuts3_keys, get_nuts3_country, nuts3_shapefile = nuts3_shapefile)
levels[levels$level == "nuts3", ]$country <- nuts3_countries

# Clear the memory
rm(nuts3_countries, nuts3_keys, nuts3_shapefile)


# Attach the countries and the levels to the df table
df <- df |>
  # Left join the user and fr country
  left_join(levels, by = c("user_loc" = "key")) |>
  left_join(levels, by = c("fr_loc" = "key"), suffix = c("_user", "_fr")) |>
  # Drop the level_fr column and rename the level_user column
  select(-c("level_fr")) |>
  rename(level = level_user)


# Filter out extra national connection and small areas
df <- df |>
  # Extra national connection
  filter(country_user == country_fr) |>
  select(-c("country_user")) |> 
  rename(country = country_fr) |>
  # Small areas and missing levels
  filter(level %in% c("gadm1", "gadm2", "nuts3"))


# Get the unique countries and the correspondent levels
country_levels <- df |> 
  group_by(country) |>
  summarise(unique(level)) |>
  rename(level = `unique(level)`)


# Drop the level column from the df table
df <- df |>
  select(-c("level"))



# =====================================================================
#                Create the network for each country
# =====================================================================


# Custom sorting functions
sort_gadm1 <- function(vec) {
  # Extract alphabetic and numeric parts
  alphabetic_part <- gsub("\\d", "", vec)  # Remove digits
  numeric_part <- as.integer(gsub("\\D", "", vec))  # Remove non-digits and convert to integer
  
  # Sort by alphabetic part first, then numeric part
  order <- order(alphabetic_part, numeric_part)
  
  # Return sorted vector
  return(vec[order])
}

sort_gadm2 <- function(vec) {
  # Extract the numbers after the underscore
  codes <- as.integer(sub(".*_(\\d{1,3})$", "\\1", vec))

  # Sort by the numbers
  order <- order(codes)
  
  # Return sorted vector
  return(vec[order])
}


# Get the shapefile for the NUTS3 regions
nuts3_shapefile <- eurostat::get_eurostat_geospatial(nuts_level = 3, year = 2016, output_class = "sf")


# Loop over the countries
for(i in 1:nrow(country_levels)){
  # Get the country and the level and subset the df
  country <- country_levels$country[i]
  level <- country_levels$level[i]
  df_country <- df[df$country == country,]

  # Extract the counties/regions
  nodeCode <- unique(c(df_country$user_loc, df_country$fr_loc))

  ## GADM ##
  if(level %in% c("gadm1", "gadm2")){
    # Sort the counties/regions, get the gadm data, create the node dataframe
    if(level == "gadm1"){
      nodeCode <- sort_gadm1(nodeCode)
      gadm_data <- gadm(country, level = 1, path = "./data/task_44/", version="3.6")
      
      nodes_df <- data.frame(nodeCode = nodeCode)
      nodes_df$nodeID <- as.integer(gsub("\\D", "", nodes_df$nodeCode))
    } else if(level == "gadm2"){
      nodeCode <- sort_gadm2(nodeCode)
      gadm_data <- gadm(country, level = 2, path = "./data/task_44/", version="3.6")
      
      nodes_df <- data.frame(nodeCode = nodeCode)
      nodes_df$nodeID <- as.integer(sub(".*_(\\d{1,3})$", "\\1", nodes_df$nodeCode))
    }

    # Extract the name
    if (level == "gadm1"){
      nodes_df$name <- gadm_data$NAME_1[nodes_df$nodeID]
    } else if (level == "gadm2"){
      nodes_df$name <- gadm_data$NAME_2[nodes_df$nodeID]
    }

    # Get the centroids of each region
    centroids <- terra::centroids(gadm_data)
    coords <- terra::crds(centroids)

    # Attach the coordinates
    nodes_df$longitude <- coords[, 1][nodes_df$nodeID]
    nodes_df$latitude <- coords[, 2][nodes_df$nodeID]

    # Get the nodeID for the user_loc and fr_loc
    nodeID_from <- sapply(df_country$user_loc, function(x) nodes_df[nodes_df$nodeCode == x, ]$nodeID)
    nodeID_to <- sapply(df_country$fr_loc, function(x) nodes_df[nodes_df$nodeCode == x, ]$nodeID)

    # Create the edge dataframe
    edges_df <- data.frame(nodeID_from = nodeID_from, nodeID_to = nodeID_to)

    # Extract the scaled_sci, the country and the country ISO3
    if (weight) edges_df$weight <- df_country$scaled_sci
    edges_df$country_name <- gadm_data$NAME_0[nodes_df$nodeID]
    edges_df$country_ISO3 <- country

    # Drop nodeCode (assuming label = name)
    if (dropNodeCode) nodes_df$nodeCode <- NULL
  }


  ## NUTS3 ##
  else if(level == "nuts3"){
    # Extract nodeID
    nodes_df <- data.frame(nodeCode = nodeCode)
    nodes_df$nodeID <- 1:nrow(nodes_df)

    # Extract the regions of the country
    nuts3_regions <- nuts3_shapefile[match(nodeCode, nuts3_shapefile$id), ]

    # Attach the name
    nodes_df$name <- nuts3_regions$NAME_LATN

    # Extract and attach the coordinates
    centroids <- st_centroid(nuts3_regions$geometry)
    coords <- st_coordinates(centroids)
    nodes_df$longitude <- coords[, 1]
    nodes_df$latitude <- coords[, 2]

    # Get the nodeID for the user_loc and fr_loc
    nodeID_from <- sapply(df_country$user_loc, function(x) nodes_df[nodes_df$nodeCode == x, ]$nodeID)
    nodeID_to <- sapply(df_country$fr_loc, function(x) nodes_df[nodes_df$nodeCode == x, ]$nodeID)

    # Create the edge dataframe
    edges_df <- data.frame(nodeID_from = nodeID_from, nodeID_to = nodeID_to)

    # Extract the scaled_sci, the country and the country ISO3
    if (weight) edges_df$weight <- df_country$scaled_sci
    country_name <- countrycode::countrycode(country, "iso3c", "country.name.en")
    edges_df$country_name <- country_name
    edges_df$country_ISO3 <- country

    # Drop nodeCode (assuming label = name)
    if (dropNodeCode) nodes_df$nodeCode <- NULL
  }

  # Save the data
  write.csv(nodes_df, file = sprintf("./data/task_44/networks/%s_nodes.csv", country), row.names = FALSE)
  write.csv(edges_df, file = sprintf("./data/task_44/networks/%s_edges.csv", country), row.names = FALSE)
}
