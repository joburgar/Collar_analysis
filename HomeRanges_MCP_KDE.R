### CREATING MCP AND AKDE HOME RANGES ###
# Code by J. Buskirk, updated March 2025


# Written based on CLEANED GPS collar data from Lotek Litetrack collars, but
#   can be easily used for any GPS collar data.
#   Just need to have latitude & longitude columns, a combined datetime column,
#   and unique collar or animal IDs
#   Can include any other desired columns/variables, such as elevation, 
#   temperature, sex, season, species, etc.

# remove anything unnecessary from Environment
rm(list=ls())

# set working directory of folder including cleaned GPS collar csv (ctrl+shift+H)

# load packages
library(amt)
library(sp)
library(sf)
library(terra)
library(raster)
library(tidyverse)
library(dplyr)

# Load CLEANED collar data
cleaned_data.frame <- read.csv("cleaned_PEPE_GPS_250324.csv", head=T) #change as needed
head(cleaned_data.frame)

# Be sure DateTime is formatting correctly
cleaned_data.frame$timestamp <- as.POSIXct(strptime(as.character(cleaned_data.frame$DateTimePST), 
                                     format = "%Y-%m-%d %H:%M:%S")) #change as needed
head(cleaned_data.frame)

# MAKE DATAFRAME INTO A TRACK
tracks <- mk_track(cleaned_data.frame, #data object
                 .x=Longitude.x,         #column name containing x
                 .y=Latitude.x,        #column name containing y
                 .t=timestamp,  #column name containing POSITx_datetime (t)
                 id= collarID,       #unique identifier
                 Temp=Temperature.C.,
                 Elev=Altitude.m.,
                 crs=4326) %>% 
  transform_coords(26910) #transform to UTMs
  
head(tracks)
class(tracks)
summary(tracks)

# CREATE MINIMUM CONVEX POLYGONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# Nest the tracks by individual
nested_tracks_mcp <- tracks %>%
  nest(data = c(x_, y_, t_, Temp, Elev))  # everything except ID

# Calculate MCPs (100%) for each individual
# Change the value under levels if you desire less than 100% MCP
mcp100 <- lapply(nested_tracks_mcp$data, hr_mcp, levels = 1.0)

# Convert MCPs to a single sf object
mcp_sf <- hr_to_sf(mcp100, id = nested_tracks_mcp$id)

# Export shapefile
st_write(mcp_sf, "MCP_100_byID_250325.shp", delete_dsn = TRUE)


# CREATE AUTOCORRELATED KERNEL DENSITY ESTIMATES ~~~~~~~~~~~~~~~~~~~~~~~~~~####

# order by id and then by timestamp
tracks <- tracks[with(tracks, order(id, t_)), ]

# make nested tracks data frame
#   need to nest by ID at least, but pull out whatever additional 
#   individual-specific covariates you want (e.g., sex, season, species)
nested_tracks<-tracks %>%
  nest(data = - c("id")) 
unique(nested_tracks$id)

# Autocorrelated KDE HR code as a loop
# Here, I use the "OUf" model, Ornstein-Uhlenbeck Foraging model, which is
#   most appropriate for territorial, resident animals and high temporal 
#   resolution (e.g., frequent fixes)

KDEs <- list()
for (i in 1:length(nested_tracks$data)) {
  print(i)
  # Fit KDE
  KDEs[[i]] <- amt::hr_akde(x = nested_tracks$data[[i]],
                             levels = c(0.50,0.95,0.99),
                             keep.data = TRUE,
                             h = hr_kde_ref(nested_tracks$data[[i]]),
                             trast = make_trast(nested_tracks$data[[i]]),
                             model= fit_ctmm(nested_tracks$data[[i]], model="ouf"))
}


# ~~~~~~~~ Save the UDs

# Export the UD raster for each collarID
for (i in seq_along(KDEs)) {
  id <- nested_tracks$id[i]
  ud_rast <- KDEs[[i]]$ud  # Use the already-generated raster
  
  if (terra::ncell(ud_rast) == 0) {
    message("Skipping ", id, ": No cell values in raster.")
    next
  }
  # Export the raster
  terra::writeRaster(ud_rast, filename = paste0("UD_", id, "_AKDE_OUF_250326.tif"), overwrite = TRUE)
}

# ~~~~~~~~ Save the SHAPEFILES

#Export KDEs as shapefiles for each contour level and each collarID
hrs <- hr_to_sf(KDEs, id = nested_tracks$id)

# Export all contours (50%, 95%, 99%) together in one shapefile
st_write(hrs, "AKDE_OUF_PEPE_250325.shp", delete_dsn = TRUE)

