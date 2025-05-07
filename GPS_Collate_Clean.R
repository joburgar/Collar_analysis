#COLLATING AND CLEANING GPS COLLAR DATA
# Code by J. Buskirk, updated March 2025

# Written based on raw Lotek Litetrack collar text files, but can be adapted for
#   any collar data
# Also uses collar metadata file, which needs the following fields for each
#   collar text file:
#   collarID, 
#   AnimalID (e.g., PEPE_ID),
#   Cptr_Date (date of animal capture),
#   Rls_Date (date of animal release, if different from capture),
#   DropDate (date collar is dropped off or removed from animal),
#   Cptr_Easting (UTM E of each animal capture locations),
#   Cptr_Northing (UTM N of each animal capture locations)

#remove anything unnecessary from Environment
rm(list=ls())

#set Working Directory of folder including ONLY the raw GPS text files
#  (ctrl+shift+H)

#Load Packages
library(stringr)
library(dplyr)
library(lubridate)
library(rgdal)
library(sp)
library(ggplot2)
library(sf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINE THE DATA AND LABEL BY COLLAR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# read in metadata file & ensure variables are in desired formats
meta <- read.csv("C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/PEPE_Metadata.csv", head=T)
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

# Set the path to your GPS collar data folder
# !!! Be sure your folder has each RAW text file of GPS collar data ONLY
# !!! Be sure not to rename the files
folder_path <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/DataToClean" #change as needed

# List all files in the folder
file_list <- list.files(folder_path, pattern = ".txt", full.names = TRUE)

# Function to read and process each file (add collarID by file name)
process_file <- function(file_path) {
  # Extract 5-digit numerical code from the file name
  collarID <- sub(".*\\D(\\d{5}).*", "\\1", basename(file_path))
  # Read the lines of the file
  file_lines <- readLines(file_path)
  # Use gsub to remove leading spaces from each line
  trimmed_lines <- gsub("^\\s+", "", file_lines)
  # Combine the trimmed lines into a single string
  file_content <- paste(trimmed_lines, collapse = "\n")
  # Read the preprocessed content into a data frame
  data <- read.table(text = file_content, header = TRUE, fill = TRUE)
  # Add the "collarID" column
  data$collarID <- collarID
  return(data)
}

# Iterate through each file and process it
result_list <- lapply(file_list, process_file)

# Combine all processed data frames into one
final_data <- do.call(rbind, result_list)

# Inspect the final combined data frame
head(final_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Save the uncleaned version ####
# Save here if you DON'T want to clean anything (e.g., if relating activity data
#   to valid/missing location data)

# Set the path to the output CSV file
output_file <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/GPScollarData_Full_Uncleaned_250324.txt" #change as needed
# Write the data frame to a CSV file
write.table(final_data, file = output_file, row.names = TRUE)
# Print a message indicating the file has been saved
cat("Data has been saved to:", output_file, "\n")

# If you want to save the combined data frame back to a text file, you can do it here
# For example, you can use write.table(final_data, "path/to/output/folder/output_file.txt", sep = " ", row.names = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CLEAN THE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#necessary for visualization, creating home ranges, etc.

# Filter rows based on Valid & HDOP < 10
# HDOP = horizontal dilution of precision
cleaned_data <- final_data %>%
  filter(Status == "Valid", HDOP <= 10)
head(cleaned_data)

# Remove any "test" points that would be before deployment on an animal

# Assuming RTC.date is in a standard date format, convert it to a Date object
# Lotek generally uses y/m/d, adjust for other collar types as necessary
cleaned_data$RTC.date <- as.Date(cleaned_data$RTC.date, format = "%y/%m/%d")

# Calculate the time difference within each CollarID
cleaned_data <- cleaned_data %>%
  group_by(collarID) %>%
  arrange(RTC.date) %>%
  mutate(TimeDiff = c(0, diff(RTC.date)))

# Remove grouping to use select
cleaned_data <- cleaned_data %>% ungroup()

# Filter out rows where the time difference is greater than 30 days
# Can adjust based on how soon before deployment you tested your collars
cleaned_data <- cleaned_data %>%
  filter(TimeDiff <= 30)

# Remove the TimeDiff column
cleaned_data <- dplyr::select(cleaned_data, -TimeDiff)

# Print or inspect the cleaned data frame
print(cleaned_data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Add UTMs ####

# ensure each Index value is unique
cleaned_data <- cleaned_data %>%
  mutate(Index = row_number())

head(cleaned_data)
tail(cleaned_data)
x <- cleaned_data$Longitude
y <- cleaned_data$Latitude

#Locations(lat-long)
Locations = subset(cleaned_data,select= c(Index, Longitude, Latitude))
head(Locations)
str(Locations)

#Convert lat long to UTMS with function
LatLongToUTM <- function(Longitude,Latitude,Index,zone){
  xy <- data.frame(Index=Index, Longitude=Longitude, Latitude=Latitude)
  coordinates(xy) <- c("Longitude", "Latitude")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone, "ellps=WGS84", sep='')))
  return(as.data.frame(res))
}

#Run the function
x=LatLongToUTM(Locations$Longitude, Locations$Latitude, Index=Locations$Index, 10)

#Merge new UTM coordinates to the Locations dataframe
cleaned_data_UTMs = merge(cleaned_data, x, by="Index")
head(cleaned_data_UTMs)
cleaned_data_UTMs <- cleaned_data_UTMs %>%
  rename(Easting = Longitude.y, Northing = Latitude.y)
head(cleaned_data_UTMs)

#~~~~~~~~~~~~~~~~~~~~~~~ calculate velocity and remove velocity > 400m/min ####
# Or determine velocity threshold to thin by based on what you think exceeds
#   your study animal's movement ability and more likely indicates error

# combine DateTime; convert UTC to PST
# create DateTime
cleaned_data_UTMs <- cleaned_data_UTMs %>%
  mutate(DateTime = as.POSIXct(paste(FIX.date, FIX.time), 
                               format = "%y/%m/%d %H:%M:%OS"))
head(cleaned_data_UTMs)

# convert UTC to PST
cleaned_data_UTMs <- cleaned_data_UTMs %>%
  mutate(DateTimePST = force_tz(DateTime, 
                                tz = "America/Los_Angeles") - hours(7))
head(cleaned_data_UTMs)

# Create DatePST and TimePST columns
cleaned_data_UTMs <- cleaned_data_UTMs %>%
  mutate(DatePST = as.Date(DateTimePST),
         TimePST = format(as.POSIXct(DateTimePST), "%H:%M:%S"))

# View result
head(cleaned_data_UTMs)

# Calculate Velocity
cleaned_data_velocity <- cleaned_data_UTMs %>%
  arrange(collarID, DateTimePST) %>%
  group_by(collarID) %>%
  mutate(
    Time_Diff = lead(DateTimePST) - DateTimePST,
    Dist_Diff = sqrt((lead(Easting) - Easting)^2 + (lead(Northing) - Northing)^2),
    Velocity = Dist_Diff / as.numeric(Time_Diff) # Velocity in meters per minute
  ) %>%
  ungroup()

summary(cleaned_data_velocity$Velocity)

READY_data <- cleaned_data_velocity %>%
  filter(Velocity < 400) #change as needed

summary(READY_data$Velocity)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Remove points beyond collar removal / drop off ####
str(READY_data)
str(meta)

# Join DropDate from metadata onto READY_data
READY_data <- READY_data %>%
  left_join(meta %>% select(collarID, DropDate), by = "collarID")

# Filter out rows where DatePST is on or after DropDate
READY_data <- READY_data %>%
  filter(DatePST < DropDate)

# Visually examine the data real quick!!!!
ggplot(READY_data, aes(x = Easting, y = Northing, color = factor(collarID))) +
  geom_point() +
  labs(title = "Location Plot by Collar ID",
       x = "Easting", y = "Northing", color = "Collar ID") +
  theme_minimal()

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Remove any additional erroneous far off points ####

# Join Cptr_Easting and Cptr_Northing to READY_data
READY_data <- READY_data %>%
  left_join(meta %>% select(collarID, Cptr_Easting, Cptr_Northing), by = "collarID")

# Calculate distance from capture location
READY_data <- READY_data %>%
  mutate(DistFromCapture = sqrt((Easting - Cptr_Easting)^2 + (Northing - Cptr_Northing)^2))
summary(READY_data$DistFromCapture)

# Filter out points > 20,000 meters away (or whatever distance you're
#   certain is beyond each animal's home range)
READY_data <- READY_data %>%
  filter(DistFromCapture <= 20000)

#Visually examine the data again!!!!

ggplot(READY_data, aes(x = Easting, y = Northing, color = factor(collarID))) +
  geom_point() +
  # Add black stars for capture locations
  geom_point(data = meta, 
             aes(x = Cptr_Easting, y = Cptr_Northing), 
             shape = 8, color = "black", size = 3) +
  labs(title = "Location Plot by Collar ID",
       x = "Easting", y = "Northing", color = "Collar ID") +
  theme_minimal()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Don't forget to add AnimalID if you want ####

# Extract from metadata
READY_data <- READY_data %>%
  left_join(meta %>% select(collarID, PEPE_ID), by = "collarID")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Double check the file is ready for writing ####
# excel will automatically convert Sats column to dates...
READY_data$Sats <- paste0("'", READY_data$Sats)
head(READY_data)

# Check the structure of the data frame
str(READY_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE THE CLEANED VERSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#As CSV SPREADSHEET
# Set the path to the output CSV file
output_file <- "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/cleaned_PEPE_GPS_250324.csv"
# Write the data frame to a CSV file
write.csv(READY_data, file = output_file, row.names = FALSE)
# Print a message indicating the file has been saved
cat("Data has been saved to:", output_file, "\n")

#As SHAPEFILE
#Remove anything unnecessary (adjust as desired; easier for Arc to handle)
str(READY_data)
READY_data_shp <- READY_data %>%
  select(-c(RTC.date, RTC.time, FIX.date, FIX.time, Delta.s., eRes, Voltage.V.,
            Time_Diff, Dist_Diff, DropDate, DistFromCapture))

str(READY_data_shp)

# Convert to an sf object
READY_sf <- st_as_sf(READY_data_shp, coords = c("Easting", "Northing"), crs = 32610)  # Replace 32610 with your correct UTM zone

# Save as a shapefile
st_write(READY_sf, "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/cleaned_PEPE_GPS_250324.shp", delete_layer = TRUE)


#~~~~~******* CHECK IT! ********~~~~~####
# Don't forget to plot and visually examine the data for any further 
#   potential errors / bad fixes

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE COLLAR METRICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# Add collar metrics to metadata for future reference
# including % fix success of total collar life
fix_success <- final_data %>%
  group_by(collarID) %>%
  summarise(FixSuccess = sum(Status == "Valid", na.rm = TRUE) / n())

# and number of used locations by animal
use_points <- READY_data %>%
  group_by(collarID) %>%
  summarise(UsePoints = n())

# Join into metadata
meta <- meta %>%
  left_join(fix_success, by = "collarID") %>%
  left_join(use_points, by = "collarID")
head(meta)

# Save updated metadata

write.csv(meta, "C:/Users/kmoriarty/Documents/MACA21_220601/BC_Fisher/updated_collar_metadata_25034.csv", row.names = FALSE)
    