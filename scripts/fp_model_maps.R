
library(raster)

shaded_fp_raster <- raster("gis_data/all_shady_fp.asc")
fifty_fp_raster <- raster("gis_data/fifty_shade_fp.asc")
sun_fp_raster <- raster("gis_data/all_sun_fp.asc")

treepalBinary <- colorRampPalette(c("goldenrod1", "gray25"))


png("plots/shaded_fp_map.png", height = 1000*8, width = 1000*8, res= 72*8)
par(bty = "n",
    cex = 1.5)
plot(shaded_fp_raster, col = treepalBinary(2))
dev.off()

png("plots/fifty_fp_map.png", height = 1000*8, width = 1000*8, res= 72*8)
par(bty = "n",
    cex = 1.5)
plot(fifty_fp_raster, col = treepalBinary(2))
dev.off()

png("plots/sunny_fp_map.png", height = 1000*8, width = 1000*8, res= 72*8)
par(bty = "n",
    cex = 1.5)
plot(sun_fp_raster, col = treepalBinary(2)[1])
dev.off()

