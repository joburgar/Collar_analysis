### IDENTIFY SPATIAL RESTING CLUSTER AREAS ###
# Code by J. Buskirk, updated March 2025



# remove anything unnecessary from Environment
rm(list=ls())

# set working directory of folder including cleaned GPS collar csv and
#   collar metadata (ctrl+shift+H)

# load packages
library(sf)
library(dplyr)

# read in metadata file & ensure variables are in desired formats
meta <- read.csv("final_collar_metadata_250329.csv", head=T)
head(meta)

# Load CLEANED collar data
cleaned_data.frame <- read.csv("cleaned_PEPE_GPS_250324.csv", head=T) #change as needed
head(cleaned_data.frame)

# Be sure DateTime is formatting correctly
cleaned_data.frame$timestamp <- as.POSIXct(strptime(as.character(cleaned_data.frame$DateTimePST), 
                                                    format = "%Y-%m-%d %H:%M:%S")) #change as needed
head(cleaned_data.frame)

# AD-HOC GPS ONLY METHOD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#    This method uses GPS only, determines resting AREAS based on time spent in
#    a given location. Does NOT consider activity data in this method.

# Convert GPS data to spatial object
gps_sf <- st_as_sf(cleaned_data.frame,
                   coords = c("Longitude.x", "Latitude.x"),
                   crs = 4326) |>  # Replace with appropriate CRS if needed
  st_transform(crs = 32610)  # Use UTM zone suitable for your area (this is UTM zone 10N)

# Buffer by desired area (3 - 10 m based on needs, I generally do 3 or 5)
#   A smaller buffer is more conservative
buffered <- st_buffer(gps_sf, dist = 3)

# Dissolve by collarID
dissolved <- buffered |>
  group_by(collarID) |>
  summarize(do_union = FALSE) |>  # Ensure no multipart
  st_cast("POLYGON")  # Cast to individual polygons

# Spatial join to GPS points
joined <- st_join(gps_sf, dissolved, join = st_within, left = FALSE)

# Count points per polygon and filter
# I generally do at least 3 hours of use for a resting cluster (this is at least
#   36 points at a 5-min fix rate, or 12 points at a 15-min fix rate)
intersections <- sf::st_intersects(dissolved, gps_sf)
n_points <- lengths(intersections)
dissolved_with_counts <- dissolved |>
  mutate(n_points = n_points) |>
  filter(n_points > 12)

#~~~~~~~~ Add desired METADATA to shapefile table

# Make sure collarID is the same type in both (just in case)
meta$collarID <- as.character(meta$collarID)
dissolved_with_counts$collarID <- as.character(dissolved_with_counts$collarID)

# Join meta info to spatial data
dissolved_with_counts <- dissolved_with_counts |>
  left_join(meta |> dplyr::select(collarID, PEPE_ID, Sex, Age_Class), by = "collarID")

# Add column to determine minimum known resting time at each cluster
dissolved_with_counts <- dissolved_with_counts |>
  mutate(RestTime = n_points * 15)  # in minutes

#~~~~~~~~ SAVE THE CLUSTER SHAPEFILE

st_write(dissolved_with_counts, "PEPE_clusters_3hr_3m_250326.shp") #change as needed

