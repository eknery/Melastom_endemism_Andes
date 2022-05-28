setwd("C:/Users/eduar/Documents/GitHub/melastom_Andes")

# packages
library(raster)
library(sf)
library(sp)
library(rgeos)

#loading coordinates
spp_points=read.table("1_thinned_data/melastom_coords_7km.csv", header =T, sep=",",  na.strings = "NA", fill=T)
str(spp_points)

# loading altitude raster
altitude = raster("C:/Users/eduar/Desktop/rasters Neotropico 2.5/altitude")

######################## defining Andes ############################
# overall long-lat extent
plot(altitude, xlim=c(-80,-62), ylim=c(-60,15)) # ANDES + tip of guiana shield

# to exclude guiana shield
# xlim=c(-80,-68)
# ylim=c(0,15)) 

######################### classifying Andean species ################

# filtering by long-lat
andean_long = spp_points[spp_points$Longitude > -80 & spp_points$Longitude < -62,]
andean_long_lat = andean_long[andean_long$Latitude > -60 & andean_long$Latitude < 15,]

# excluding guiana-shield
minus_guiana = c(0,0,0)
for (i in 1:nrow(andean_long_lat) ){
  entry = andean_long_lat[i,]
  if (entry$Latitude > 0 & entry$Latitude < 15 & entry$Longitude > -68){next}
  minus_guiana = rbind(minus_guiana, entry)
}
minus_guiana = minus_guiana[-1,]

# by altitude
spp_alt = raster::extract(altitude, minus_guiana[,2:3])
andean_samples = minus_guiana[spp_alt > 1200,]

# plotting
plot(altitude)
points(andean_samples[,2:3], cex=0.4)

# spp with any occurrence in the Andes
spp_in_andes = unique(andean_samples$species)

# occurrences in the andes
n_in_andes = aggregate(andean_samples$species, by=list(andean_samples$species), length)

# total occurrences from spp in the andes
spp_in_andes_points = spp_points[spp_points$species %in% spp_in_andes,]
n_total = aggregate(spp_in_andes_points$species, by=list(spp_in_andes_points$species), length)

# percentage in andes
percentage_andes = round(n_in_andes[,2] / n_total[,2],2)

# andean species
andean_spp = spp_in_andes[percentage_andes > 0.75]
andean_spp_points = spp_points[spp_points$species %in% andean_spp,]

# removing NAs
andean_samples = andean_samples[- which(is.na(andean_samples[,2]) | is.na(andean_samples[,3]) ),]
andean_spp_points[- which( is.na(andean_spp_points[,2]) | is.na(andean_spp_points[,3]) ),]

# ploting
plot(altitude)
points(andean_samples[,2:3], cex=0.4)
points(andean_spp_points[,2:3], col= "blue", cex=0.4)

# exporting
write.table(andean_samples, "2_andean_data/andean_samples.csv", sep=',', row.names = F, quote=F)
write.table(andean_spp_points, "2_andean_data/andean_spp_points.csv", sep=',', row.names = F, quote=F)

######################## keeping only andean points in andean spp ############################

### keeping only andean points
# setting Andean boundaries
andean_long_bound = range(andean_samples$Longitude)
andean_lat_bound = range(andean_samples$Latitude)

# filter by long-lat
long_filter_spp = andean_spp_points[andean_spp_points$Longitude >= andean_long_bound[1] & andean_spp_points$Longitude <= andean_long_bound[2],]
long_lat_filter_spp = long_filter_spp[long_filter_spp$Latitude >= andean_lat_bound[1] & long_filter_spp$Latitude <= andean_lat_bound[2],]

# taking guiana-shield occurrences off
minus_guiana = c(0,0,0)
for (i in 1:nrow(long_lat_filter_spp) ){
  entry = long_lat_filter_spp[i,]
  if (entry$Latitude > 0 & entry$Latitude < 15 & entry$Longitude > -68){next}
  minus_guiana = rbind(minus_guiana, entry)
}
minus_guiana = minus_guiana[-1,]

# filter altitude
alt_values = raster::extract(altitude, minus_guiana[,2:3])
andean_spp_points_in = minus_guiana[alt_values > 1200,]

# export
write.table(andean_spp_points_in,"2_andean_data/andean_spp_points_in.csv", sep=',', row.names= F, quote=F)

