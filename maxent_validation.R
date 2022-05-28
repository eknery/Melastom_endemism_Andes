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

### loading raster layers
ras1 = raster("rasters_2.5min/temperature_warmest_quarter")
ras2 = raster("rasters_2.5min/temperature_seasonality")
ras3 = raster("rasters_2.5min/precipitation_driest_quarter")
ras4 = raster("rasters_2.5min/precipitation_seasonality")
env_ras= stack(ras1,ras2,ras3, ras4)

altitude = raster("rasters_2.5min/altitude")

############################ MaxEnt validation  ########################

### defining spp to be modeled
spp_occ_count = aggregate(andean_spp_points$species, by=list(andean_spp_points$species), length)
all_spp_names = spp_occ_count$Group.1[spp_occ_count$x >= 6]

### background to sp format
bg_sp = bg_points
coordinates(bg_sp) =  ~Longitude+Latitude
crs(bg_sp) = crs(env_ras)

### cross validation
k_value = 3

### list of threshold values per species
all_spp_ths = vector('list', length(all_spp_names))
names(all_spp_ths) = all_spp_names

### looping over species
for (sp_name in all_spp_names){
  sp_points = andean_spp_points[andean_spp_points$species == sp_name,2:3]
  # k-fold
  sp_folds = kfold(sp_points, k=k_value)
  sp_th_values = rep(NA, k_value)
  for (i in 1:k_value){
    sp_train = sp_points[sp_folds != i,]
    sp_test = sp_points[sp_folds == i,]
    # modeling
    me = maxent(x=env_ras, p=sp_train, a=bg_points, removeDuplicates=T)
    # creating pseudo-absences = background - sp occurrences
    circ_around <- circles(sp_train, d= 5000, lonlat=TRUE)
    poly_around <- polygons(circ_around)
    virtual_abs = erase.point(bg_sp, poly_around, inside = TRUE)
    # evaluating model
    eval_me = evaluate(model=me, p=sp_test, a=virtual_abs, x=env_ras) 
    sp_th_values[i] = as.numeric(threshold(eval_me)[2])
  }
  all_spp_ths[[sp_name]] = sp_th_values
  n = which(sp_name == all_spp_names)
  print(n)
}

spp_ths_df = c(0,0,0,0)
for (sp_name in all_spp_names){
  one_entry = c(sp_name, all_spp_ths[sp_name][[1]])
  spp_ths_df = rbind(spp_ths_df, one_entry)
}
spp_ths_df = spp_ths_df[-1,]

write.table(spp_ths_df, "4_maxent_validation/spp_ths_df.csv", sep=',', quote=F, row.names=F)
