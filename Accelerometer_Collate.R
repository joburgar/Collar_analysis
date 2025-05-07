#COLLATING AND CLEANING COLLAR ACCELEROMETER DATA
# Code by J. Buskirk, updated March 2025

# Written based on raw Lotek Litetrack collar csv files as output from 
#   converting .adf files. Can be used for Biaxial or Triaxial data.
# Also uses collar metadata file

# This process does NOT clean the data (e.g., does NOT remove activity data
#   beyond collar dropoff). This cleaning can take place simultaneously with
#   raw GPS and activity data files during future analyses (e.g., creating
#   behavior states). Can also use this data beyond dropoff to help calibrate
#   behavior states (e.g., collar is still for XX days and then moves on
#   highway).

#remove anything unnecessary from Environment
rm(list=ls())

#set Working Directory (ctrl+shift+H) of parent folder including:
#  collar metadata csv
#  GPS text file subfolder
#  Accelerometer csv file subfolder

#Load Packages
library(stringr)
library(dplyr)
library(lubridate)
library(rgdal)
library(sp)
library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINE THE DATA AND LABEL BY COLLAR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# Set the path to your folder
# !!! Be sure your folder has each RAW csv file of GPS accelerometer data ONLY
# !!! Be sure not to rename the files
folder_path <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/AccelerometerToClean" #update as needed

# List all files in the folder
file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Function to process each file
process_file <- function(file_path) {
  # Extract collarID from file path
  collarID <- sub(".*\\D(\\d{5}).*", "\\1", file_path)
  # Read file, skipping first 4 rows
  data <- read_csv(file_path, skip = 4)
  # Add collarID column
  data$collarID <- collarID
  return(data)
}

# Process each file and combine into a single dataframe
combined_data <- bind_rows(lapply(file_list, process_file))
head(combined_data)

# Multiply X, Y columns by 0.31392 to scale values to meters/sec^2 (BIAXIAL)
combined_data <- mutate(combined_data, X_m_s2 = X * 0.31392, Y_m_s2 = Y * 0.31392)
# Multiply X, Y, and Z columns by 0.31392 to scale values to meters/sec^2
#combined_data <- mutate(combined_data, X_m_s2 = X * 0.31392, Y_m_s2 = Y * 0.31392, Z_m_s2 = Z * 0.31392) #if have z for TRIAXIAL

# Reorder the columns
combined_data <- combined_data %>%
  select(collarID, `GMT Time`, `Temperature [C]`, X, Y, X_m_s2, Y_m_s2) #Add Z and Z_m_s2 if TRIAXIAL

# Print or inspect the combined data frame
head(combined_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Add metadata ####

# read in metadata file & ensure variables are in desired formats
meta <- read.csv("C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/PEPE_Metadata.csv", head=T) #update as needed
head(meta)
str(meta)
meta <- meta %>%
  mutate(
    collarID = as.character(collarID),
    Cptr_Date = as.Date(Cptr_Date, format = "%d-%b-%y"),
    Rls_Date = as.Date(Rls_Date, format = "%d-%b-%y"),
    DropDate = as.Date(DropDate, format = "%d-%b-%y")
  )
str(meta)

#Add animal ID
READY_data <- combined_data %>%
  left_join(meta %>% select(collarID, PEPE_ID), by = "collarID")
head(READY_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SAVE THE FULL VERSION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# Set the path to the output CSV file
output_file <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/ActivityPEPEdata_Full_250325.csv" #update as needed
# Write the data frame to a CSV file
write.csv(READY_data, file = output_file, row.names = TRUE)
# Print a message indicating the file has been saved
cat("Data has been saved to:", output_file, "\n")

#SAVE BIAXIAL VS TRIAXIAL VERSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# If have triaxial data in the future, can still iterate through the above
#   process and then save each version below (based on data use needs)

# Filter data with NAs under Z (Biaxial)
data_with_NA <- combined_data %>% filter(is.na(Z))
output_file <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/ActivityPEPEdata_Biaxial_250325.csv" #update as needed
write.csv(data_with_NA, file = output_file, row.names = TRUE)

# Filter data without NAs under Z (Triaxial)
data_without_NA <- combined_data %>% filter(!is.na(Z))
output_file <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/ActivityPEPEdata_Triaxial_250325.csv" #update as needed
write.csv(data_without_NA, file = output_file, row.names = TRUE)
