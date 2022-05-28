setwd("C:/Users/eduar/Documents/GitHub/melastom_Andes")

# packages
library(raster)
library(sf)
library(sp)
library(rgeos)
library(dismo)

#loading background
melastom_coords = read.table("0_raw_data/melastom_coords_IDonly.csv", header =T, sep=",",  na.strings = "NA", fill=T)

#loading andean species - only points in the Andes
andean_spp_points_in =read.table("2_andean_data/andean_spp_points_in.csv", header =T, sep=",",  na.strings = "NA", fill=T)

# loading altitude raster
altitude = raster("rasters_2.5min/altitude")

# null raster
null_raster = !is.na(altitude)
null_raster[null_raster==0] = NA
plot(null_raster)

################################### counting points in sites #####################

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
record_counts = point_count(x= null_raster, points= melastom_coords[,2:3])
record_counts = (record_counts / null_raster)

### constraining counts to the Andes
# altitude
andes_record_counts = record_counts
andes_record_counts[altitude < 1200] = 0
# andean extent
ras_cell_xy = data.frame(coordinates(andes_record_counts))
non_andean_long = which(ras_cell_xy$x < -80 | ras_cell_xy$x > -62)
non_andean_lat = which(ras_cell_xy$y < -60 | ras_cell_xy$y > 15)
andes_record_counts[c(non_andean_long, non_andean_lat)] = 0
# take out guiana
guina_cells = which(ras_cell_xy$y > 0 & ras_cell_xy$y  < 15 & ras_cell_xy$x > -68)
andes_record_counts[guina_cells] = 0
# border 
andes_record_counts = (andes_record_counts / null_raster)

# exporting
writeRaster(andes_record_counts, filename = "7_sampling_effort/andes_record_counts", overwrite=T)

####################################### plotting ##############################

andes_record_counts = raster("7_sampling_effort/andes_record_counts")

# plotting parameters
myframe = raster::extent(as.matrix(andean_spp_points_in[,2:3]))*1.3

# aggregating factor
factor_values = c(1,4,8,12)

# maximum count
max_agg = aggregate(andes_record_counts, fact= max(factor_values), fun='sum')
max_value = cellStats(max_agg, stat='max')

# color legend
mycolors = c("yellowgreen", "green4")
col_func = colorRampPalette(mycolors) 
legend_colors= col_func(max_value) 
legend_colors[1] = "gray85"

# count in different resolutions
plot_list = list()
for(i in 1:length(factor_values)){
  # aggregating counts
  one_count = disaggregate(aggregate(andes_record_counts, fact= factor_values[i], fun=sum), fact= factor_values[i])
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
  tiff(paste("7_sampling_effort/sampling_effort_maps/andes_record_counts", as.character(factor_values[i]), ".tiff", sep=""), units="in", width=4.5, height=6, res=600)
  replayPlot(plot_list[[i]])
  dev.off()
}

#### maximum scale raster 
andes_records_max_scale = disaggregate(aggregate(andes_record_counts, fact= max(factor_values), fun='sum'), fact= max(factor_values) )
andes_records_max_scale = (andes_records_max_scale/null_raster)

writeRaster(andes_records_max_scale, "7_sampling_effort/andes_records_max_scale", overwrite=T)

######################### sampling effort vs spp richness ###################

library(tidyverse)

# loading sampling effort
andes_records_max_scale = raster("7_sampling_effort/andes_records_max_scale")
# loading empirical spp richness
spp_richness_max_scale = raster("3_spp_richness/spp_richness_max_scale")
# loading projected spp richness
andes_bin_max_scale = raster ("5_maxent_projections/andes_bin_projections_max_scale")
# loading differences
diff_max_scale = raster("6_richness_difference/diff_max_scale")

# n spp in sampled sites
n_spp = spp_richness_max_scale[andes_records_max_scale > 0]
# n spp in sampled sites
n_proj = andes_bin_max_scale[andes_records_max_scale > 0]
# n records in sampled sites
n_record = andes_records_max_scale[andes_records_max_scale > 0]
# n diff
n_diff = diff_max_scale[andes_records_max_scale > 0]

df = data.frame(n_record, n_spp, n_proj, n_diff)

tiff("7_sampling_effort/sampled_spp_per_record.tiff", units="in", width=4.5, height=4.5, res=600)
ggplot(data= df, aes(x=n_record, y=n_spp)) +
  geom_point(color="steelblue1", size = 2, alpha = 0.6) +
  geom_smooth(method="lm", formula =y ~ poly(x, 2), se= T, level=0.95, color="slateblue4", size=1.25,  alpha=0.25)+
  xlim(c(0,max(n_record)))+
  xlab("n records")+ ylab("n sampled species")+
  theme(panel.background=element_rect(fill="white"), panel.grid=element_line(colour=NULL),panel.border=element_rect(fill=NA,colour="black"),axis.title=element_text(size=14,face="bold"),axis.text=element_text(size=10),legend.position = "none")
dev.off()

mycol_func = colorRampPalette(c("yellow1", "orangered"))
diff_colors = mycol_func(range(n_diff)[2])

tiff("7_sampling_effort/inferred_vs_sampled_spp.tiff", units="in", width=4.5, height=4.5, res=600)
ggplot(data= df, aes(x=n_spp, y=n_proj, fill=as.factor(n_diff) )) +
  geom_point( aes(color=as.factor(n_diff)), size = 2, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color="darkred", linetype="dashed", size=1.5)+
  scale_color_manual(values=diff_colors) +
  xlim(c(0,max(n_proj))) +ylim(c(0,max(n_proj)))+
  xlab("n sampled species")+ ylab("n inferred species")+
  theme(panel.background=element_rect(fill="white"), panel.grid=element_line(colour=NULL),panel.border=element_rect(fill=NA,colour="black"),axis.title=element_text(size=14,face="bold"),axis.text=element_text(size=10),legend.position = "none")
dev.off()

