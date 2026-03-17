library(dplyr)
library(lubridate)

# Assuming df_clean is your cleaned data frame with lon, lat, time, sst

# Convert time column to Date class if not already
df_clean$time <- as.Date(df_clean$time)

# Extract year from the time column
df_clean$year <- year(df_clean$time)

# 1. Calculate average sst per year per lon/lat
avg_sst_yearly <- df_clean %>%
  group_by(lon, lat, year) %>%
  summarise(avg_sst = mean(sst, na.rm = TRUE)) %>%
  ungroup()

# 2. Calculate max sst per year per lon/lat
max_sst_yearly <- df_clean %>%
  group_by(lon, lat, year) %>%
  summarise(max_sst = max(sst, na.rm = TRUE)) %>%
  ungroup()

# Optionally save these to CSV files
write.csv(avg_sst_yearly, "avg_sst_per_year_red_sea.csv", row.names = FALSE)
write.csv(max_sst_yearly, "max_sst_per_year_red_sea.csv", row.names = FALSE)


###


# Load required libraries
library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)

# Define the processing function
process_sst_for_years <- function(years) {
  # Define Red Sea bounding box
  lat_min <- 12.5
  lat_max <- 30.0
  lon_min <- 32.0
  lon_max <- 45.0
  
  for (yr in years) {
    cat("Processing year:", yr, "\n")
    
    # Construct file name
    file_name <- paste0("sst.day.mean.", yr, ".nc")
    
    # Check if file exists
    if (!file.exists(file_name)) {
      cat("File not found:", file_name, "- skipping this year.\n\n")
      next
    }
    
    # Open NetCDF file
    nc_data <- nc_open(file_name)
    
    # Extract coordinates
    lat <- ncvar_get(nc_data, "lat")
    lon <- ncvar_get(nc_data, "lon")
    time <- ncvar_get(nc_data, "time")
    
    # Find indices within Red Sea bounds
    lat_idx <- which(lat >= lat_min & lat <= lat_max)
    lon_idx <- which(lon >= lon_min & lon <= lon_max)
    
    # Extract sst variable
    sst <- ncvar_get(nc_data, "sst")
    
    # Confirm dimension order
    dims <- dim(sst)
    cat("Dimensions of sst variable:", paste(dims, collapse = " x "), "\n")
    
    # Subset sst by lon and lat indices, keep all time
    # Assuming sst is [lon, lat, time]
    sst_subset <- sst[lon_idx, lat_idx, ]
    
    # Subset coordinates
    lat_subset <- lat[lat_idx]
    lon_subset <- lon[lon_idx]
    
    # Convert time units to dates
    time_units <- ncatt_get(nc_data, "time", "units")$value
    origin_date <- sub("days since ", "", time_units)
    dates <- as.Date(time, origin = origin_date)
    
    nc_close(nc_data)
    
    # Create long-format data frame
    df <- expand.grid(lon = lon_subset, lat = lat_subset, time = dates)
    df$sst <- as.vector(sst_subset)
    
    # Remove NA values
    df_clean <- df[!is.na(df$sst), ]
    
    # Add year column extracted from time
    df_clean$year <- year(df_clean$time)
    
    # Calculate average sst per lon, lat, year
    avg_sst_yearly <- df_clean %>%
      group_by(lon, lat, year) %>%
      summarise(avg_sst = mean(sst, na.rm = TRUE), .groups = "drop")
    
    # Calculate max sst per lon, lat, year
    max_sst_yearly <- df_clean %>%
      group_by(lon, lat, year) %>%
      summarise(max_sst = max(sst, na.rm = TRUE), .groups = "drop")
    
    # Save CSV files in working directory
    avg_file <- paste0("avg_sst_per_year_red_sea_", yr, ".csv")
    max_file <- paste0("max_sst_per_year_red_sea_", yr, ".csv")
    
    write.csv(avg_sst_yearly, avg_file, row.names = FALSE)
    write.csv(max_sst_yearly, max_file, row.names = FALSE)
    
    cat("Saved average SST to:", avg_file, "\n")
    cat("Saved max SST to:", max_file, "\n\n")
  }
}

# Specify the years you want to process
years_to_process <- c(2000, 2001, 2003, 2014, 2017)

# Run the processing function
process_sst_for_years(years_to_process)