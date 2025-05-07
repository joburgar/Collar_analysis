### EXAMINE HABITAT RELATIONSHIPS ###
# Code by J. Buskirk, updated March 2025

# Process remotely sensed spatial layers as needed
# Examine habitat relationships using:
#   Manly-Chesson selection indices
#   Logistic regression models (r.g., RSFs) - TBD, not added as of March 31, 2024

# remove anything unnecessary from Environment
rm(list=ls())

# set working directory of folder including cleaned GPS collar csv, collar 
#   metadata, home range polyons (here, I'm using 95% AKDE.OUFs),
#   and any habitat layers (I keep my layers in a subfolder called GIS_data)
#   (ctrl+shift+H)
#     Here, I'm using cut block polygons, canopy raster, wildfire kmz,
#     OTHERS?!
setwd("C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher")

# load packages
library(sf)
library(raster)
library(terra)
library(tidyverse)
library(lwgeom)
library(dplyr)
library(ggplot2)

# READ AND PROCESS DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# Read in animal data
kdes <- st_read("AKDE_OUF_PEPE_250325.shp")
gps <- st_read("cleaned_PEPE_GPS_250324.shp")

# Read in remotely sensed data
cut <- st_read("GIS_data/CUT_BLOCKS_clip.shp") #cut blocks POLYGONS
canopy <- raster("GIS_data/canopy_cover_2022.tif") #canopy cover RASTER

# BC fire polygons are provided as KMZs by year
# Loop through converting and combinbing them to shapefile here
fire_kmz_paths <- list.files("GIS_data/Fire", pattern = "\\.kmz$", full.names = TRUE)
read_kmz_as_sf <- function(kmz_path) {
  unzip(kmz_path, exdir = tempdir())  # KMZ is just a zipped KML
  kml_file <- list.files(tempdir(), pattern = "\\.kml$", full.names = TRUE)[1]
  st_read(kml_file)
}
fire_list <- lapply(fire_kmz_paths, read_kmz_as_sf)
fire <- do.call(rbind, fire_list)

# CLIP habitat layers to animal home ranges ~~~~~~~~

# VECTORS first.......
cut <- st_transform(cut, st_crs(kdes)) # Ensure CRS matches
fire <- st_transform(fire, st_crs(kdes))

# Clip to kdes
cut_clipped <- st_intersection(cut, kdes)
fire_clipped <- st_intersection(fire, kdes)

# Now any RASTERS.......
# Reproject kdes to match raster CRS
kdes_raster_crs <- st_transform(kdes, crs(canopy))

# Convert to Spatial (required for raster masking)
kdes_sp <- as(kdes_raster_crs, "Spatial")

# Crop and mask
canopy_crop <- crop(canopy, kdes_sp)
canopy_clipped <- mask(canopy_crop, kdes_sp)


# MANLY-CHESSON SELECTION INDICES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# MCSI's are a simple way to examine whether animals avoid or select for a
#   given variable (e.g., clearcuts, fire severities, young stands, etc.).
#   Resultant values > 1 indicate selection FOR, < 1 indicate AVOIDANCE, and 
#   values = 1 indicate neither / no preference.

# ?? Do recent CUT BLOCKS (10 years or newer) impact fisher movement ?? 
str(kdes)
str(gps)
str(cut_clipped)

kdes_95 <- kdes %>%   # Filter to 95% estimates only
  filter(level == 0.95, what == "estimate") %>%
  mutate(area_ha = as.numeric(st_area(geometry)) / 10000)  # Convert m² to ha

cut_recent <- cut_clipped %>%   # Filter to recent cuts only
  filter(HARVEST_MI > 2013)

# ~~~~~~~~~~~~~~~~~~~~~~~ Calculate area of CUT in each home range ~~~~~~~~
cut_recent <- st_transform(cut_recent, st_crs(kdes_95)) # Ensure CRS match

# Join by spatial intersection
cut_per_kde <- st_intersection(cut_recent, kdes_95)

# Calculate area of cut polygons
cut_per_kde <- cut_per_kde %>%
  mutate(cut_area_ha = as.numeric(st_area(geometry)) / 10000)

# Summarize total cut area per animal
cut_summary <- cut_per_kde %>%
  group_by(name) %>%
  summarise(total_cut_ha = sum(cut_area_ha, na.rm = TRUE))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Count GPS points per home range ~~~~~~~~
gps <- st_transform(gps, st_crs(kdes_95)) # Ensure CRS match

# Join GPS points to KDEs
gps_with_kde <- st_join(gps, kdes_95, join = st_within)

# Count points per KDE
gps_per_kde <- gps_with_kde %>%
  group_by(name) %>%
  summarise(total_gps = n())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Count GPS points in CUT blocks ~~~~~~~~
# Join GPS with recent cut blocks
gps_in_cut <- st_join(gps, cut_recent, join = st_within)

# Filter only those with a match (i.e., not NA)
gps_in_cut_filtered <- gps_in_cut %>%
  filter(!is.na(HARVEST_MI))

# Join again with kdes to get animal name
gps_in_cut_named <- st_join(gps_in_cut_filtered, kdes_95, join = st_within)

# Count used points in cut per animal
gps_cut_summary <- gps_in_cut_named %>%
  group_by(name = name.y) %>%
  summarise(gps_in_cut = n())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Calculate MCSI ~~~~~~~~
manly_df <- kdes_95 %>%   # BY INDIVIDUAL
  st_drop_geometry() %>%
  select(name, area_ha) %>%
  left_join(cut_summary, by = "name") %>%
  left_join(gps_per_kde, by = "name") %>%
  left_join(gps_cut_summary, by = "name") %>%
  mutate(
    prop_available = total_cut_ha / area_ha,
    prop_used = gps_in_cut / total_gps,
    manly_chesson = prop_used / prop_available
  )

mean_index <- manly_df %>%   # AVERAGED
  summarise(
    avg_prop_available = sum(total_cut_ha, na.rm = TRUE) / sum(area_ha, na.rm = TRUE),
    avg_prop_used = sum(gps_in_cut, na.rm = TRUE) / sum(total_gps, na.rm = TRUE)
  ) %>%
  mutate(manly_chesson_avg = avg_prop_used / avg_prop_available)

manly_df
mean_index

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot MCSIs ~~~~~~~~

MCSI_pop <- ggplot(manly_df, aes(x = "", y = manly_chesson)) +
  geom_boxplot(fill = "lightblue", color = "black", width = 0.2, outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 1.2)) +
  labs(
    x = "Population level (all fishers)",
    y = "Manly-Chesson Selection Index",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
MCSI_pop

ggsave("MCSI_CutBlocks_Boxplot_250327.png", MCSI_pop, width = 5, height = 5, dpi = 300)
