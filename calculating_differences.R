setwd("C:/Users/eduar/Documents/GitHub/melastom_Andes")

### loading dataset and packages
# packages
library(raster)
library(sf)
library(sp)

#loading andean species 
andean_spp_points_in = read.table("2_andean_data/andean_spp_points_in.csv", header =T, sep=",",  na.strings = "NA", fill=T)

# loanding empirical spp richness
spp_richness_max_scale = raster("3_spp_richness/spp_richness_max_scale")

# loading projected spp richness
andes_bin_max_scale = raster ("5_maxent_projections/andes_bin_projections_max_scale")

### difference between projection and empirical
diff_max_scale = andes_bin_max_scale - spp_richness_max_scale 

# exporting
writeRaster(diff_max_scale, filename = "6_richness_difference/diff_max_scale", overwrite=T)

################################### plotting full differences ################################

diff_max_scale = raster("6_richness_difference/diff_max_scale")

### difference values
diff_range = cellStats(diff_max_scale,'range')
diff_values = diff_range[1]:diff_range[2]

# spatial frame
myframe = raster::extent(as.matrix(andean_spp_points_in[,2:3]))*1.3

# color legend
mycol_func = colorRampPalette(c("yellow1", "orangered"))
legend_colors = mycol_func(diff_range[2])
legend_colors[which(diff_values == 0)] = "gray85"

tiff("6_richness_difference/richness_difference_maps/diff_richness_max_scale.tiff", units="in", width=4.5, height=6, res=600)
  plot(diff_max_scale, col= legend_colors, colNA="white", ext=myframe)
dev.off()

#### plotting differences in sampled sites 
sampled_diff = diff_max_scale
sampled_diff[spp_richness_max_scale == 0] = 0
sampled_diff = (sampled_diff/ null_raster)

tiff("6_richness_difference/richness_difference_maps/sampled_diff_richness.tiff", units="in", width=4.5, height=6, res=600)
  plot(sampled_diff, col= legend_colors, colNA="white", ext=myframe)
dev.off()
