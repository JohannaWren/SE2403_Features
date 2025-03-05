# Script to start looking at oceanographic data during the cruise

# Libraries
library(dplyr)
library(readxl)
library(stringr)

##-------------------------------------------------------------
## Read in the data
##-------------------------------------------------------------
# Nutrients
nutrients <- readxl::read_xls('~/Documents/Cruises/SE2403_Features/Data/Himmelsbach_240620.xls', skip=12)
nutrients <- nutrients[-1,]
str(nutrients)
nut <- cbind(matrix(unlist(strsplit(nutrients$Sample, split='-')), ncol = 2, byrow = T), nutrients[,-1])
colnames(nut)[1:2] <- c('StationID', 'Depth')
nut$Ammonia <- ifelse(nut$Ammonia == "<0.019", 0.0189, as.numeric(nut$Ammonia))
nut$Phosphate <- as.numeric(nut$Phosphate)
nut$Silicate <- as.numeric(nut$Silicate)
nut$`N+N` <- as.numeric(nut$`N+N`)
nut$Depth <- as.numeric(nut$Depth)
nut

# CTD
CTDlog <- read.csv('CTDlog.csv') %>% 
  mutate(DateTimeHST=as.POSIXct(DateTimeHST, format='%m/%d/%y %H:%M')) %>% 
  filter(MaxDepth_m > 1000)

# merge ctdlog and nutrients
# add station ID to the ctd log
idx <- paste(rep(2:16, each=2), rep(1:2), sep='.')
idx[1] <- '2.3'
idx[2] <- '2.4'
idx <- idx[-21]
CTDlog$StationID <- idx
nutri <- full_join(CTDlog[,c(4:6,8:10)], nut)
nutri

# Plot
# nutriDN <- nutri %>% filter(Day.Night == 'Day', Location == 'Cyclonic 1')
# 
# estimate_temp_by_date <- function(taget_lat, target_depth) {
#   data_for_date <- nutriDN %>% 
#     filter(Lat == taget_lat) %>%
#     arrange(Depth)
#   
#   # approx() is one way to do a linear interpolation
#   approx(data_for_date$Depth, data_for_date$`N+N`, xout = target_depth)$y
# }
# 
# temp_interp_depth <- crossing(
#   # the same dates as sonde_tbl_1993
#   tibble(Lat = unique(nutriDN$Lat)),
#   # depths can now be any value
#   tibble(Depth = seq(from=0, to=200, by=5))
# ) %>%
#   group_by(Lat) %>%
#   mutate(NN = estimate_temp_by_date(Lat[1], Depth))
# 
# # create a function that will, given a depth, estimate the temp on any given day
# estimate_temp_by_depth <- function(target_depth, target_lat) {
#   data_for_depth <- temp_interp_depth %>% 
#     filter(Depth == target_depth) %>%
#     arrange(Lat)
#   approx(data_for_depth$Lat, data_for_depth$NN, xout = target_lat)$y
# }
# 
# temp_raster <- crossing(
#   # dates can now be any value
#   tibble(Lat = seq(max(nutriDN$Lat), min(nutriDN$Lat), by = -0.1)),
#   # depths must be the same as in temp_interp_depth
#   tibble(Depth = unique(temp_interp_depth$Depth))
# ) %>%
#   group_by(Depth) %>%
#   mutate(NN = estimate_temp_by_depth(Depth[1], Lat))
# 
# ggplot(temp_raster, aes(Lat, Depth, fill = NN)) +
#   geom_raster() +
#   scale_y_reverse() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = FALSE)
#-----------------------

library(MBA)
library(reshape2)
library(ggplot2)
nutriDN <- nutri %>% 
  mutate(Depth = Depth) %>% 
  filter(Day.Night == 'Day', Location == 'Anti-Cyclonic') %>% 
  dplyr::select(Lat, Depth, Silicate) %>% 
  rename(NutVar=Silicate)

ctd_mba <- mba.surf(na.omit(nutriDN), no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Lat', 'Depth'), value.name = 'NutVar') %>% 
  #filter(Depth < 0) %>% 
  mutate(NutVar = round(NutVar, 1))

ggplot(data = ctd_mba, aes(x = Lat, y = Depth)) +
  geom_raster(aes(fill = NutVar)) +
  scale_fill_viridis_c(option = 'mako') +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
  geom_contour(aes(z = NutVar), breaks = 20, colour = "black") +
  ### Activate to see which pixels are real and not interpolated
  geom_point(data = nutriDN, aes(x = Lat, y = Depth),
             colour = 'black', size = 0.2, alpha = 0.4, shape = 8) +
  ###
  labs(y = "Depth (m)", x = 'Latitude', fill = "Silicate \n(µmol/L)") +
  coord_cartesian(expand = 0) +
  ggtitle('Anticyclonic eddy, Day sampling', subtitle = 'Interpolated over depth and space just to give an idea of patterns.\nBlack dots show actual sampling locations.Used lat instead of station numbers')

ggsave('SE2403_anticyclonic_Silicate_Day_cleaned.png', width=6, height=8)
