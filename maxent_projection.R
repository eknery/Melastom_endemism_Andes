setwd("C:/Users/eduar/Documents/GitHub/melastom_Andes")

### loading dataset and packages
# packages
library(raster)
library(sf)
library(sp)
library(rgeos)
library(dismo)
library(spatialEco)
library(rJava)

#loading andean species 
andean_spp_points = read.table("2_andean_data/andean_spp_points.csv", header =T, sep=",",  na.strings = "NA", fill=T)
str(andean_spp_points)

# loading andean species with only andean occurrences
andean_spp_points_in = read.table("2_andean_data/andean_spp_points_in.csv", header =T, sep=",",  na.strings = "NA", fill=T)
str(andean_spp_points_in)

#loading background
bg_points = read.table("1_thinned_data/bg_points.csv", header =T, sep=",",  na.strings = "NA", fill=T)

# loading threshold values per species
spp_ths_df = read.table("4_maxent_validation/spp_ths_df.csv", header =T, sep=",",  na.strings = "NA", fill=T)


### loading raster layers
ras1 = raster("rasters_2.5min/temperature_warmest_quarter")
ras2 = raster("rasters_2.5min/temperature_seasonality")
ras3 = raster("rasters_2.5min/precipitation_driest_quarter")
ras4 = raster("rasters_2.5min/precipitation_seasonality")
env_ras= stack(ras1,ras2,ras3, ras4)

altitude = raster("rasters_2.5min/altitude")

# null raster
null_raster = !is.na(altitude)
null_raster[null_raster==0] = NA
plot(null_raster)

############################## Maxent projection ##########################

### defining spp to be modeled
spp_occ_count = aggregate(andean_spp_points$species, by=list(andean_spp_points$species), length)
all_spp_names = spp_occ_count$Group.1[spp_occ_count$x >= 6]

# mean threshold per species
med_ths = apply(spp_ths_df[-1], MARGIN=1, FUN=median)
spp_med_ths = data.frame(spp_ths_df[,1], med_ths)
colnames(spp_med_ths) = c("species", "ths")

### modeling all species
all_projections_full = all_projections_bin = vector('list', length(all_spp_names))
names(all_projections_full) = names(all_projections_bin) = all_spp_names

for (i in 1:length(all_spp_names)){
  sp_name = all_spp_names[i]
  sp_points = andean_spp_points[andean_spp_points$species == sp_name,]
  # fitting maxent model
  me = maxent(x=env_ras, p=sp_points[,2:3], a=bg_points, removeDuplicates=T)
  # full projection
  one_projection = predict(me, env_ras)
  bin_projection = one_projection >= spp_med_ths[spp_med_ths$species == sp_name,2]
  all_projections_bin[[i]] = bin_projection
  print(i)
}

### summing individual bin projections
sum_bin_projections = all_projections_bin[[1]]
for (i in 2:length(all_spp_names)){
  sum_bin_projections = sum_bin_projections + all_projections_bin[[i]]
}

# exporting
writeRaster(sum_bin_projections, filename = "5_maxent_projections/sum_bin_projections")


### constraining projections to the Andes
# altitude
andes_bin_projections = sum_bin_projections
andes_bin_projections[altitude < 1200] = 0
# andean extent
ras_cell_xy = data.frame(coordinates(andes_bin_projections))
non_andean_long = which(ras_cell_xy$x < -80 | ras_cell_xy$x > -62)
non_andean_lat = which(ras_cell_xy$y < -60 | ras_cell_xy$y > 15)
andes_bin_projections[c(non_andean_long, non_andean_lat)] = 0
# take out guiana
guina_cells = which(ras_cell_xy$y > 0 & ras_cell_xy$y  < 15 & ras_cell_xy$x > -68)
andes_bin_projections[guina_cells] = 0
# border 
andes_bin_projections = (andes_bin_projections / null_raster)

# exporting
writeRaster(andes_bin_projections, filename = "5_maxent_projections/andes_bin_projections")

############################### plotting binary projections - Andes only ####################################

# loading binary projections
andes_bin_projections = raster("5_maxent_projections/andes_bin_projections")

# max value
max_value = cellStats(andes_bin_projections, stat='max')

# spatial frame
myframe = raster::extent(as.matrix(andean_spp_points_in[,2:3]))*1.3

mycol_func = colorRampPalette(c("lightpink", "darkred"))
legend_colors= mycol_func(max_value)
legend_colors[1] = "gray85"

# aggregating factor
factor_values = c(1,4,8,12)

plot_list_3 = list()
for(i in 1:length(factor_values)){
  agg_projections = disaggregate(aggregate(andes_bin_projections, fact= factor_values[i], fun='max'), fact= factor_values[i])
  agg_projections = (agg_projections/null_raster) 
  # range of probs
  max = raster::cellStats(agg_projections, stat='max')
  relative_color_range = legend_colors[1:max]
  # make plot
  plot(agg_projections ,col=relative_color_range , colNA="white", ext=myframe)
  one_plot = recordPlot()
  plot_list_3[[i]] = one_plot
  plot.new()
}

# plotting
for (i in 1:length(factor_values)){
  tiff(paste("5_maxent_projections/andes_bin_projection_maps/andes_bin_projection", as.character(factor_values[i]), ".tiff", sep=""), units="in", width=4.5, height=6, res=600)
  replayPlot(plot_list_3[[i]])
  dev.off()
}

#### maximum scale raster
andes_bin_projections_max_scale = disaggregate(aggregate(andes_bin_projections, fact= max(factor_values), fun='max'), fact= max(factor_values) )
andes_bin_projections_max_scale = (andes_bin_projections_max_scale/null_raster)

writeRaster(andes_bin_projections_max_scale, "5_maxent_projections/andes_bin_projections_max_scale", overwrite=T)

