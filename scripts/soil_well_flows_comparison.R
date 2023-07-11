##
## Flow at soil wells comparison across model scenarios
##

library(stringr)
library(xts)
library(zoo)
library(lubridate)

well_file_names <- paste0("soil_well_", c(1:14, 17:21), ".csv")
well_flow_file_names <- paste0("soil_well_", c(1:14, 17:21), "_flow.csv")
file_path <- "C:\\Users\\skati\\OneDrive - Montana State University\\All_none_fifty_analysis\\"


all_sun_soil_flow_nasa <- mapply(function(x) read.table(paste0(file_path, "all_sun\\nasa_air\\", x), col.names = c("head", "pressure_head", "saturation","x", "y", "z", "zsurf", "elem"), sep = ","),
                            well_flow_file_names,
                            SIMPLIFY = F)
all_sun_soil_flow_meacham <- mapply(function(x) read.table(paste0(file_path, "all_sun\\meacham_air\\", x), col.names = c("head", "pressure_head", "saturation","x", "y", "z", "zsurf", "elem"), sep = ","),
                                 well_flow_file_names,
                                 SIMPLIFY = F)
no_sun_soil_flow_nasa <- mapply(function(x) read.table(paste0(file_path, "no_sun\\nasa_air\\", x), col.names = c("head", "pressure_head", "saturation","x", "y", "z", "zsurf", "elem"), sep = ","),
                                 well_flow_file_names,
                                 SIMPLIFY = F)
no_sun_soil_flow_meacham <- mapply(function(x) read.table(paste0(file_path, "no_sun\\meacham_air\\", x), col.names = c("head", "pressure_head", "saturation","x", "y", "z", "zsurf", "elem"), sep = ","),
                                    well_flow_file_names,
                                    SIMPLIFY = F)

saturations <- data.frame(well = 0, sat_nasa_sun = 0, sat_meacham_sun = 0, sat_nasa_shade = 0, sat_meacham_shade = 0)
wellnumbers <- c(1:14, 17:21)

for(i in 1:length(wellnumbers)){
  saturations[i,1] <- wellnumbers[i]
  saturations[i,2] <- round(all_sun_soil_flow_nasa[[i]]$saturation,2)
  saturations[i,3] <- round(all_sun_soil_flow_meacham[[i]]$saturation,2)
  saturations[i,4] <- round(no_sun_soil_flow_nasa[[i]]$saturation,2)
  saturations[i,5] <- round(no_sun_soil_flow_meacham[[i]]$saturation,2)
}
saturations
