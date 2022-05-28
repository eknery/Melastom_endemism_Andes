
########################## overall melastom background  ###########################

# packages
library(raster)
library(sf)
library(sp)
library(rgeos)

# loading all melastom occurrences
spp_points = read.table("1_thinned_data/melastom_coords_7km.csv", sep=",", h=T)

# loading altitude raster
altitude = raster("C:/Users/eduar/Desktop/rasters Neotropico 2.5/altitude")

# removing points outside raster extent
extracted = raster::extract(env_ras[[1]], spp_points[,2:3])
bg_points = spp_points[-which(is.na(extracted)),2:3]

#export
write.table(bg_points, "1_thinned_data/bg_points.csv", sep=",", row.names = F, quote = F)
