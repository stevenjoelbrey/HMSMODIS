################################################################################
# modisAnalysis.R
#
# Datasource: https://firms.modaps.eosdis.nasa.gov/download/
# 
# 
#
# TODO: Would want to clean up a bit of the hourly differences here. 
################################################################################
library(maps)
library(geosphere) # for haversine calculation 

minYear <- 2010
maxYear <- 2011
minDate <- as.POSIXct(paste0(minYear, "-01-01"), tz="UTC")
maxDate <- as.POSIXct(paste0(maxYear, "-12-31"), tz="UTC")

# Load modis data
modis_df <- read.csv("Data/fire_archive_M6_10936_all.csv", 
                     stringsAsFactors = FALSE)
Date <- as.POSIXct(modis_df$acq_date, tz="UTC")
modis_df$Date <- Date

# Load hms detections 
hms_df <- get(load("Data/hysplitPoints_land_both.RData"))

# Match these dataframes time range of interest 
hms_mask <- hms_df$Date >= minDate & hms_df$Date <= maxDate
hms_df <- hms_df[hms_mask, ]
# Get coords to correct numeric class 
hms_df$Lat <- as.numeric(hms_df$Lat)
hms_df$Lon <- as.numeric(hms_df$Lon)

# We also do not care about Canada or Alaska
latMask <- hms_df$Lat < 50 
hms_df <- hms_df[latMask,]

# Create columns for attaching modis data to each hysplit point
hms_df$mean_frp   <- NA
hms_df$mean_conf  <- NA
hms_df$brightness <- NA
hms_df$nWithin4   <- 0

n <- dim(hms_df)[1]

# For every hysplit point I want to know how many MODIS detections are within
# 4 km. 
for (i in 1:n){
  
  if (i %% 1000 == 0){
      # Only print sometimes, for performance
      print(paste("Percent complete:", round(i/n*100, 4) ))
  }
  
  # The point to be investigated
  pt1 <- c(hms_df$Lon[i], hms_df$Lat[i])
  
  dateMask <- hms_df$Date[i] == modis_df$Date
  modis_df_subset <- modis_df[dateMask,]

  # Now get rid of the points we know are far away 
  lonD <- abs(pt1[1] - modis_df_subset$longitude)
  latD <- abs(pt1[2] - modis_df_subset$latitude)
  distMask <- lonD < 0.06 & latD < 0.06
  
  # Only proceed if any modis detection is within this distnace
  if(sum(distMask > 0)){
    
    modis_df_subset <- modis_df_subset[distMask,]
    nClose <- dim(modis_df_subset)[1]
    
    distances <- rep(NA, nClose)
    for (j in 1:nClose){
      pt2 <- c(modis_df_subset$longitude[j], modis_df_subset$latitude[j])
      distances[j] <- distHaversine(pt1, pt2) / 1000 # to get from m to km
    }
    
    # Now hold onto the number of modis detections that are within 10 km
    distMask <- distances <= 4
    modis_close <- modis_df_subset[distMask, ]
    
    # Get the mean confidense, frp, brightness
    hms_df$mean_frp[i]   <- mean(modis_close$frp)
    hms_df$mean_conf[i]  <- mean(modis_close$confidence)
    hms_df$brightness[i] <- mean(modis_close$brightnes)
    hms_df$nWithin4[i]   <- dim(modis_close)[1]
    
  }
  
  
}

# Save the appended hysplit point dataframe 
save(hms_df, file = paste0("Data/hms_with_modis_", minYear,"_",maxYear,".RData"))



