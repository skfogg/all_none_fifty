library(raster)
library(stringr)

choptop <- raster("gis_data/choptop_2013.asc")
restoration_reach <- raster("gis_data/be2009_restorationReach.asc")
stream_channel <- raster("gis_data/stream_only_actually.asc")
soil_obs_points <- read.table("gis_data/soil_obs_points.txt",
                              col.names = c("label", "x", "y", "z"))

soil_wells <- SpatialPointsDataFrame(soil_obs_points[,2:3], 
                                     data = data.frame(soil_obs_points[,4]))

par(mfrow = c(1,1))
png("plots/well_map.png", height = 1000*8, width = 1000*8, res= 72*8)
plot(restoration_reach)
plot(stream_channel, add = T, col = "blue")
plot(soil_wells, add = T, pch = 16, col = "red")
text(soil_wells, labels = str_split(soil_obs_points$label, "_", simplify = T)[,2],
     adj = 0, halo = T, cex = 1.5)
dev.off()
