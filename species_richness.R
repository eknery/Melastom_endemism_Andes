setwd("C:/Users/eduar/Documents/GitHub/melastom_Andes")

# packages
library(raster)
library(sf)
library(sp)
library(rgeos)
library(dismo)

#loading andean species - only points in the Andes
andean_spp_points_in =read.table("2_andean_data/andean_spp_points_in.csv", header =T, sep=",",  na.strings = "NA", fill=T)
str(spp_points)

#loading andean species 
andean_spp_points = read.table("2_andean_data/andean_spp_points.csv", header =T, sep=",",  na.strings = "NA", fill=T)

# loading altitude raster
altitude = raster("rasters_2.5min/altitude")

# null raster
null_raster = !is.na(altitude)
null_raster[null_raster==0] = NA
plot(null_raster)

#################################### species richness count ###################################

### keeping spp that can be modeled
spp_occ_count = aggregate(andean_spp_points$species, by=list(andean_spp_points$species), length)
all_spp_names = spp_occ_count$Group.1[spp_occ_count$x >= 6]
andean_spp_points_in = andean_spp_points_in[andean_spp_points_in$species %in% all_spp_names,]

### point count raster function
point_count = function(x, points){
  # make a zero raster
  y = x
  y[] = 0
  # get cell index for each point and make a table:
  counts = table(cellFromXY(x, points))
  # fill raster with the counts from the cell index:
  y[as.numeric(names(counts))] = counts
  return(y)
}

# counting points in each grid cell
count_ras = point_count(x= null_raster, points= andean_spp_points_in[,2:3])
spp_richness = count_ras / null_raster
plot(spp_richness)

# exporting
writeRaster(spp_richness, "3_spp_richness/spp_richness", overwrite=TRUE)

############################# plotting at different scales ##################################

#loading raster
spp_richness = raster("3_spp_richness/spp_richness")

# plotting parameters
myframe = raster::extent(as.matrix(andean_spp_points_in[,2:3]))*1.3

# aggregating factor
factor_values = c(1,4,8,12)

# maximum count
max_agg = aggregate(andes_record_counts, fact= max(factor_values), fun='sum')
max_value = cellStats(max_agg, stat='max')

# color legend
mycolors = c("steelblue1", "slateblue4")
col_func = colorRampPalette(mycolors) 
legend_colors= col_func(max_value) #
legend_colors[1] = "gray85"

# count in different resolutions
plot_list = list()
for(i in 1:length(factor_values)){
  # aggregating counts
  one_count = disaggregate(aggregate(spp_richness, fact= factor_values[i], fun=sum), fact= factor_values[i])
  one_count = (one_count/null_raster) 
  # take range of counts
  max = raster::cellStats(one_count, stat='max')
  relative_color_range = legend_colors[1:max]
  # make plot
  plot(one_count ,col= relative_color_range, colNA="white", ext=myframe)
  one_plot <- recordPlot()
  plot_list[[i]] = one_plot
  plot.new()
}

# plotting
for (i in 1:length(factor_values)){
  tiff(paste("3_spp_richness/spp_richness_maps/spp_richness", as.character(factor_values[i]), ".tiff", sep=""), units="in", width=4.5, height=6, res=600)
  replayPlot(plot_list[[i]])
  dev.off()
}

############################### maximum scale raster #################################

spp_richness_max_scale = disaggregate(aggregate(spp_richness, fact= max(factor_values), fun=sum), fact= max(factor_values))
spp_richness_max_scale = (spp_richness_max_scale/null_raster)

writeRaster(spp_richness_max_scale, "3_spp_richness/spp_richness_max_scale", overwrite=T)
