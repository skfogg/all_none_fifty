##
## SOIL TEMPERAUTRE COMPARISON
## - Compare shady and sunny soil temperatures collected from meacham creek
##   to shady and sunny soil temperatures output in hgs
##

library(stringr)
library(xts)
library(zoo)
library(lubridate)

well_file_names <- paste0("soil_well_", c(1:14, 17:21), ".csv")
well_flow_file_names <- paste0("soil_well_", c(1:14, 17:21), "_flow.csv")
file_path <- "C:\\Users\\skati\\OneDrive - Montana State University\\All_none_fifty_analysis\\"

##### Compare sunny soil temperatures #####
all_sun_soil <- mapply(function(x) read.csv(paste0(file_path, "all_sun\\", x), col.names = c("time", "temp", "trash","x", "y", "z", "zsurf", "trash", "trash"))[,c(-3,-8,-9)],
                       well_file_names,
                       SIMPLIFY = F)
all_sun_soil_flow <- mapply(function(x) read.table(paste0(file_path, "all_sun\\", x), col.names = c("head", "pressure_head", "saturation","x", "y", "z", "zsurf", "elem"), sep = ","),
                       well_flow_file_names,
                       SIMPLIFY = F)
all_sun_soil_ts <- mapply(function(x) xts(zoo(x$temp, order.by = mdy_hms("01-01-2012 00:00:00")+ x$time)),
                       all_sun_soil,
                       SIMPLIFY = F)

### Depth of each well point under the floodplain surface ###
mapply(function(x) x[1,"zsurf"] - x[1,"z"],
       all_sun_soil)

meacham_sun_soil <- read.csv("meacham_creek_data/sunnysoil.csv")
meacham_sun_soil_ts <- xts(zoo(meacham_sun_soil$Temp, order.by = mdy_hms(meacham_sun_soil$DateTime)))

plot.zoo(all_sun_soil_ts[[1]]["2020/2021"], type = "n",
         ylim = c(-10,35))
mapply(function(x,c) lines(as.zoo(x["2020/2021"]), col = c),
       all_sun_soil_ts,
       hcl.colors(19))
lines(as.zoo(meacham_sun_soil_ts))

## Well 14 matches well with meacham during winter. * WELL 14 is in the CHANNEL & FULLY SAT *
## Well 6 gets the warmest
plot.zoo(all_sun_soil_ts[[14]]["2020/2021"],
         col = hcl.colors(19)[14],
         ylim = c(-5,35))
lines(as.zoo(all_sun_soil_ts[[6]]["2020/2021"]),
             col = hcl.colors(19)[6])
lines(as.zoo(meacham_sun_soil_ts))
legend("topleft", c("6", "14"), fill = hcl.colors(19)[c(6,14)])


##### Compare sunny soil temperatures #####
no_sun_soil <- mapply(function(x) read.csv(paste0(file_path, "no_sun\\nasa_air\\", x), col.names = c("time", "temp", "trash","x", "y", "z", "zsurf", "trash", "trash"))[,c(-3,-8,-9)],
                       well_file_names,
                       SIMPLIFY = F)
no_sun_soil_ts <- mapply(function(x) xts(zoo(x$temp, order.by = mdy_hms("01-01-2012 00:00:00")+ x$time)),
                          no_sun_soil,
                          SIMPLIFY = F)

meacham_shade_soil <- read.csv("meacham_creek_data/shadysoil.csv")
meacham_shade_soil_ts <- xts(zoo(meacham_shade_soil$Temp, order.by = mdy_hms(meacham_shade_soil$DateTime)))

plot.zoo(no_sun_soil_ts[[1]]["2020/2021"], type = "n",
         ylim = c(-10,35))
mapply(function(x,c) lines(as.zoo(x["2020/2021"]), col = c),
       no_sun_soil_ts,
       hcl.colors(19))
lines(as.zoo(meacham_shade_soil_ts))
legend("topleft", as.character(c(1:14,17:21)), fill = hcl.colors(19))


## WELL 14 Matches best with the shady soil temp taken from the meacham floodplain ##
plot.zoo(no_sun_soil_ts[[14]]["2020/2021"],
         col = hcl.colors(19)[14],
         ylim = c(-8,20))
lines(as.zoo(meacham_shade_soil_ts))


### PLOT ALL Wells (except 14) and plot the meacham temp with it:
png("plots/soil_temps_compared_to_meacham.png",
    width = 1200*8, height = 1200*8, res = 72*8)
par(mfrow = c(2,1),
    mar = c(3,3,3,1))
plot.zoo(all_sun_soil_ts[[1]]["2020/2021"], type = "n",
         ylim = c(-10,35), main = "All Sun")
mapply(function(x,c) lines(as.zoo(x["2020/2021"]), col = c),
       all_sun_soil_ts[-14],
       hcl.colors(18))
lines(as.zoo(meacham_sun_soil_ts))

plot.zoo(no_sun_soil_ts[[1]]["2020/2021"], type = "n",
         ylim = c(-10,35), main = "All Shade")
mapply(function(x,c) lines(as.zoo(x["2020/2021"]), col = c),
       no_sun_soil_ts[-14],
       hcl.colors(18))
lines(as.zoo(meacham_shade_soil_ts))
legend("topleft", as.character(c(1:13,17:21)), fill = hcl.colors(18))
dev.off()




compare_well_plots <- function(all_sun, no_sun, well_counter) {
  plot.zoo(all_sun["2020/2021"],
         col = "orange",
         ylim = c(-9, 35),
         main = paste("well", well_counter))
  lines(as.zoo(no_sun["2020/2021"]),
             col = "brown")
}

mapply(compare_well_plots,
       all_sun_soil_ts,
       no_sun_soil_ts,
       c(1:14, 17:21))  

plot.zoo(meacham_sun_soil_ts, col = "orange",
         ylim = c(-8, 35))
lines(as.zoo(meacham_shade_soil_ts), col = "brown")

## Meacham soil temp models ##
shade_soil_model <- read.table("hgs_model_input_files/floodplain_shade_inputs/shady3.txt",
                             skip = 1,
                             col.names = c("s", "e", "temp"))
sun_soil_model <- read.table("hgs_model_input_files/floodplain_shade_inputs/sunny3.txt",
                             skip = 1,
                             col.names = c("s", "e", "temp"))
shade_soil_model_ts <-xts(zoo(shade_soil_model$temp, 
                              order.by = mdy_hms("01-01-2019 00:00:00") + shade_soil_model$s))
sun_soil_model_ts <-xts(zoo(sun_soil_model$temp, 
                              order.by = mdy_hms("01-01-2019 00:00:00") + sun_soil_model$s))


time_period <- "2020-07-28/2020-09-10"
lwide <- 2
compare_everything_plots <- function(all_sun, no_sun, well_counter, soil_sat, time_period, lwide = 2) {
  png(paste("plots/sun_shade_soil_compare_well_", well_counter, "_nasa.png"),
      width = 800*8, height = 900*8, res = 72*8)
  par(mfrow = c(2,1),
      mar = c(2,2,2,1))
  mean_diffs_hgs <- apply.daily(all_sun[time_period], mean) - apply.daily(no_sun[time_period], mean)
  plot.zoo(all_sun[time_period],
           col = "orange",
           ylim = c(4, 35),
           main = paste("well", well_counter),
           lwd = lwide)
  lines(as.zoo(no_sun[time_period]),
        col = "brown",
        lwd = lwide)
  text(ymd_hms(paste(str_split(time_period, "/", simplify = T)[1,1],"00:00:00")) + (86400*6), 
       30, 
       labels = paste("soil sat =", round(soil_sat$saturation, 2)))
  text(ymd_hms(paste(str_split(time_period, "/", simplify = T)[1,1],"00:00:00")) + (86400*5),
       5, labels = paste("avg. daily mean diff =", signif(mean(mean_diffs_hgs),2)),
       pos = 4)
  
  # plot.zoo(sun_soil_model_ts[time_period], 
  #          col = "gold",
  #          ylim = c(5,35))
  # lines(as.zoo(shade_soil_model_ts[time_period]),
  #       col = "saddlebrown")
  
  mean_diffs_meacham <- apply.daily(meacham_sun_soil_ts[time_period], mean) - apply.daily(meacham_shade_soil_ts[time_period], mean)
  plot.zoo(meacham_sun_soil_ts[time_period],
           col = "gold",
           lwd = lwide,
           ylim = c(4,35), 
           xlim = c(ymd_hms(paste(str_split(time_period, "/", simplify = T)[1,1], "00:00:00")), 
                    ymd_hms(paste(str_split(time_period, "/", simplify = T)[1,2], "00:00:00"))),
           main = time_period)
  lines(as.zoo(meacham_shade_soil_ts[time_period]),
        lwd = lwide,
        col = "saddlebrown")
  text(ymd_hms(paste(str_split(time_period, "/", simplify = T)[1,1],"00:00:00")) + (86400*5),
       5, labels = paste("avg. daily mean diff =", signif(mean(mean_diffs_meacham), 2)),
       pos = 4)
  dev.off()
}


### 2020 summer ###
mapply(compare_everything_plots,
       all_sun_soil_ts,
       no_sun_soil_ts,
       c(1:14, 17:21),
       all_sun_soil_flow,
       MoreArgs = list(time_period = "2020-07-28/2020-09-10"))  

### 2021 summer ###
mapply(compare_everything_plots,
       all_sun_soil_ts,
       no_sun_soil_ts,
       c(1:14, 17:21),
       all_sun_soil_flow,
       MoreArgs = list(time_period = "2021-05-28/2021-08-15"))


tail(meacham_sun_soil_ts["2020"])
tail(meacham_shade_soil_ts["2021-05-01/"])
plot.zoo(meacham_sun_soil_ts["2020-07-30/2020-08-16"],
         col = "orange", ylim = c(10,35))
lines(as.zoo(meacham_shade_soil_ts["2020-07-30/2020-08-16"]),
      col = "saddlebrown")
plot.zoo(meacham_sun_soil_ts["2021-07-30/2021-08-16"],
         col = "orange", ylim = c(10,35))
lines(as.zoo(meacham_shade_soil_ts["2021-07-30/2021-08-16"]),
      col = "saddlebrown")

cross_over_time_21 <- "2021-07-30/2021-08-16"
daily_mean_diffs_21 <- apply.daily(meacham_sun_soil_ts[cross_over_time_21], mean) - apply.daily(meacham_shade_soil_ts[cross_over_time_21], mean)

cross_over_time_20 <- "2020-07-30/2020-08-16"
daily_mean_diffs_20 <- apply.daily(meacham_sun_soil_ts[cross_over_time_20], mean) - apply.daily(meacham_shade_soil_ts[cross_over_time_20], mean)

avg_soil_meacham_diff <- mean(c(coredata(daily_mean_diffs_20), coredata(daily_mean_diffs_21)))
### The Avg. temperature difference between sunny and shady soil temperatures from Jul-30 to Aug-16 
### is 9.5 degrees C.

soil_well_diffs <- mapply(function(x,y) apply.daily(x[cross_over_time_20], mean) - apply.daily(y[cross_over_time_20], mean),
                          all_sun_soil_ts,
                          no_sun_soil_ts,
                          SIMPLIFY = F)
mean_well_diff <- sapply(soil_well_diffs, mean)
mean(mean_well_diff - avg_soil_meacham_diff)

### Across all sample wells, over this time period, the HGS model is predicting a temperature
### reduction 2.5 degrees greater than the measured soil temperatures.

saturations <- data.frame(well = 0, sat = 0)
wellnumbers <- c(1:14, 17:21)

for(i in 1:length(wellnumbers)){
  saturations[i,2] <- round(all_sun_soil_flow[[i]]$saturation,2)
  saturations[i,1] <- wellnumbers[i]
}

write.csv(saturations, "soil_well_saturations.csv", row.names = F)
