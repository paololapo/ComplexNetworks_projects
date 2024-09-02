library(dplyr)
library(tidyr)
library(terra)
library(geodata)
library(eurostat)
library(sf)
library(tigris)
library(igraph)


# =====================================================================
#                               Nodes
# =====================================================================

# Import the table with the levels of the counties
regions <- read.table(file = "./data/task_44/meta/gadm1_nuts3_counties_levels.csv", sep = ',', header = TRUE)
regions <- regions |> rename(nodeCode = key)

get_nuts3_country <- function(nuts3_code, nuts3_shapefile){
  nuts3_region <- nuts3_shapefile[nuts3_shapefile$id == nuts3_code, ]
  country <- countrycode::countrycode(nuts3_region$CNTR_CODE, origin = "eurostat", destination = "iso3c")
  return(country)
}

# Attach the country to the levels table
regions$country_ISO3 <- sub("^([A-Z]+).*", "\\1", regions$nodeCode)

# Overwrite nuts3 countries with ISO 3 codes
nuts3_shapefile <- eurostat::get_eurostat_geospatial(nuts_level = 3, year = 2016, output_class = "sf")
nuts3_keys <- regions[regions$level == "nuts3", ]$nodeCode
nuts3_countries <- sapply(nuts3_keys, get_nuts3_country, nuts3_shapefile = nuts3_shapefile)
regions[regions$level == "nuts3", ]$country_ISO3 <- nuts3_countries


# Get the GADM1 data
gadm1_regions <- regions[regions$level == "gadm1", ]
gadm1_keys <- unique(gadm1_regions$country_ISO3)
gadm1_data <- gadm(gadm1_keys, level = 1, path = "./data/task_44/", version="3.6")
gadm1_data$nodeCode <- gsub("\\.", "", sub("_1$", "", gadm1_data$GID_1))
gadm1_data <- gadm1_data[match(gadm1_regions$nodeCode, gadm1_data$nodeCode), ]

gadm1_data <- gadm1_data[, (names(gadm1_data) %in% c("nodeCode", "NAME_1"))]

centroids <- terra::centroids(gadm1_data)
coords <- terra::crds(centroids)
gadm1_data$longitude <- coords[, 1]
gadm1_data$latitude <- coords[, 2]

rm(centroids, coords, gadm1_regions, gadm1_keys)

# Merge the data
gadm1_data <- as.data.frame(gadm1_data)
regions <- regions |> 
  left_join(gadm1_data, by = c("nodeCode" = "nodeCode")) |>
  rename(nodeLabel = NAME_1)

rm(gadm1_data)


# Get the GADM2 data
gadm2_regions <- regions[regions$level == "gadm2", ]
gadm2_keys <- unique(gadm2_regions$country_ISO3)
gadm2_data <- gadm(gadm2_keys, level = 2, path = "./data/task_44/", version="3.6")
gadm2_data$nodeCode <- gsub("(\\w+)\\.(\\d+)\\.(\\d+)_1$", "\\1\\2_\\3", gadm2_data$GID_2)

gadm2_data <- gadm2_data[, (names(gadm2_data) %in% c("nodeCode", "NAME_2"))]

centroids <- terra::centroids(gadm2_data)
coords <- terra::crds(centroids)
gadm2_data$longitude <- coords[, 1]
gadm2_data$latitude <- coords[, 2]

rm(centroids, coords, gadm2_regions, gadm2_keys)

# Convert the data to a data frame
gadm2_data <- as.data.frame(gadm2_data) |> rename(nodeLabel = NAME_2)

# Change the nodeCode to the format of the GADM2 data
gadm2_data <- gadm2_data |>
  separate(nodeCode, into = c("prefix", "n2"), sep = c("_"), remove = TRUE) |>
  separate(prefix, into = c("prefix", "n1"), sep = c("(?<=^[A-Z]{3})"), remove = TRUE) |>
  group_by(prefix) |>
  mutate(sequential_number = row_number()) |>
  ungroup() |>
  mutate(nodeCode = paste0(prefix, n1, "_", sequential_number)) |>
  select(c("nodeCode", "nodeLabel", "longitude", "latitude"))

# Merge the data
regions <- regions |> 
  left_join(gadm2_data, by = c("nodeCode" = "nodeCode"), suffix = c("_x", "_y")) |>
  mutate(nodeLabel = coalesce(nodeLabel_x, nodeLabel_y)) |>
  select(-nodeLabel_x, -nodeLabel_y) |>
  mutate(longitude = coalesce(longitude_x, longitude_y)) |>
  mutate(latitude = coalesce(latitude_x, latitude_y)) |>
  select(-longitude_x, -longitude_y, -latitude_x, -latitude_y)

rm(gadm2_data)


# Get the nuts3 data
nuts3_regions <- regions[regions$level == "nuts3", ]
nuts3_keys <- unique(nuts3_regions$nodeCode)
nuts3_shapefile <- eurostat::get_eurostat_geospatial(nuts_level = 3, year = 2016, output_class = "sf")
nuts3_data <- nuts3_shapefile[match(nuts3_keys, nuts3_shapefile$id), ]

centroids <- st_centroid(nuts3_data$geometry)
coords <- st_coordinates(centroids)
nuts3_data$longitude <- coords[, 1]
nuts3_data$latitude <- coords[, 2]

rm(centroids, coords, nuts3_regions, nuts3_keys)

# Convert the data to a data frame
nuts3_data <- as.data.frame(nuts3_data) 
nuts3_data <- nuts3_data |>
  rename(nodeLabel = NAME_LATN) |>
  rename(nodeCode = id) |>
  select(c("nodeCode", "nodeLabel", "longitude", "latitude"))

# Merge the data
regions <- regions |> 
  left_join(nuts3_data, by = c("nodeCode" = "nodeCode"), suffix = c("_x", "_y")) |>
  mutate(nodeLabel = coalesce(nodeLabel_x, nodeLabel_y)) |>
  select(-nodeLabel_x, -nodeLabel_y) |>
  mutate(longitude = coalesce(longitude_x, longitude_y)) |>
  mutate(latitude = coalesce(latitude_x, latitude_y)) |>
  select(-longitude_x, -longitude_y, -latitude_x, -latitude_y)

rm(nuts3_data)

# Get the USA data
usa_regions <- regions[regions$country_ISO3 == "USA", ]
state_fips <- substr(usa_regions$nodeCode, 4, 5)
usa_shapefile <- counties(state = state_fips, year = 2015, cb = TRUE, class = "sf")
usa_shapefile$code <- paste0("USA", usa_shapefile$STATEFP, usa_shapefile$COUNTYFP)
usa_data <- usa_shapefile[match(usa_regions$nodeCode, usa_shapefile$code), ]

centroids <- st_centroid(usa_data$geometry)
coords <- st_coordinates(centroids)
usa_data$longitude <- coords[, 1]
usa_data$latitude <- coords[, 2]

# Convert the data to a data frame
usa_data <- as.data.frame(usa_data)
usa_data <- usa_data |>
  rename(nodeLabel = NAME) |>
  rename(nodeCode = code) |>
  select(c("nodeCode", "nodeLabel", "longitude", "latitude"))

# Merge the data
regions <- regions |> 
  left_join(usa_data, by = c("nodeCode" = "nodeCode"), suffix = c("_x", "_y")) |>
  mutate(nodeLabel = coalesce(nodeLabel_x, nodeLabel_y)) |>
  select(-nodeLabel_x, -nodeLabel_y) |>
  mutate(longitude = coalesce(longitude_x, longitude_y)) |>
  mutate(latitude = coalesce(latitude_x, latitude_y)) |>
  select(-longitude_x, -longitude_y, -latitude_x, -latitude_y)

rm(usa_data)

# Clear regions from NA values
regions <- regions[!is.na(regions$nodeLabel), ]

# Add the nodeID
regions <- regions |>
  mutate(nodeID = row_number()) |>
  select(nodeID, everything())

# Save the data
write.csv(regions, "./data/task_44/networks/full_world_nodes.csv", row.names = FALSE)



# =====================================================================
#                               Edges
# =====================================================================

# Load the data
regions <- read.csv("./data/task_44/networks/full_world_nodes.csv")

# Import the data from meta
path_data_fb <- "./data/task_44/meta/gadm1_nuts3_counties-gadm1_nuts3_counties - FB Social Connectedness Index - October 2021.tsv"
df <- read.table(file = path_data_fb, sep = "\t", header = TRUE)
print(paste0("Size of df: ", format(object.size(df), units = "auto")))

# Filter out unknown node codes
df <- df |>
  filter(user_loc %in% regions$nodeCode & fr_loc %in% regions$nodeCode)

# Add the nodeIDs
df <- df |>
  # Add the user nodeID
  left_join (regions |> select(nodeID, nodeCode), by = c("user_loc" = "nodeCode")) |>
  rename(nodeID_from = nodeID) |>
  # Add the friend nodeID
  left_join (regions |> select(nodeID, nodeCode), by = c("fr_loc" = "nodeCode")) |>
  rename(nodeID_to = nodeID) |>
  # Drop the nodeCode columns
  select(nodeID_from, nodeID_to, scaled_sci) |>
  rename(weight = scaled_sci)

# Save the data
write.csv(df, "./data/task_44/networks/full_world_edges.csv", row.names = FALSE)
