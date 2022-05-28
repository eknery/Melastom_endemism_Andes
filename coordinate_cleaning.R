setwd("C:/Users/eduar/Desktop")

### loading package for data cleanning 
install.packages("CoordinateCleaner")
library(CoordinateCleaner)
library(raster)

### loading raw dataset
ds = read.table("Ocurrences_2_bar_delimited.txt",sep = "|", header = T, dec = ",", as.is=T, fill=T)
original_size = nrow(ds)

### removing records in the sea
ds_1 = cc_sea(ds, lon = "decimalLongitude", lat = "decimalLatitude",  scale = 110, value = "clean", speedup = T)
size_1 =  nrow(ds_1)

### removing invalid coordinates
ds_2 = cc_val(ds_1, lon = "decimalLongitude", lat = "decimalLatitude", value = "clean")
size_2 = nrow(ds_2)

### removing coordinates outside reported country
ds_3 = cc_cen(ds_2, lon = "decimalLongitude", lat = "decimalLatitude", species = "acceptedScientificName", buffer = 2000, geod = TRUE, test = "both", ref = NULL, verify = FALSE, value = "clean")
size_3 = nrow(ds_3)

### removing duplicates by collector and collector number
ds_4 = cc_dupl(ds_3, lon = "decimalLongitude", lat = "decimalLatitude", species = "acceptedScientificName", additions = c("recordedBy","recordNumber"), value = "clean", verbose = TRUE )
nrow(ds_4)

### removing duplicates by county and date
ds_5 = cc_dupl(ds_4, lon = "decimalLongitude", lat = "decimalLatitude", species = "acceptedScientificName", additions = c("county","year", "month", "day"), value = "clean", verbose = TRUE )
nrow(ds_5)

### removing records with unknown identifier
ds_6 = ds_5[which(ds_5$identifiedBy == ""),]

### visualizing
par(mfrow=c(1,2))
plot(ds_5$decimalLongitude, ds_5$decimalLatitude)
plot(ds_6$decimalLongitude, ds_6$decimalLatitude)

### processing all dataset
# simplifying species name
sp_name_list = strsplit(ds_5$acceptedScientificName, split=" ")
species = c()
for (i in 1:length(sp_name_list) ){
  all_name = sp_name_list[[i]]
  species = c(species, paste(all_name[1], all_name[2], sep="_") )
}

# organizing into simple dataframe
melastom_coords_full = data.frame(species, ds_5$decimalLongitude, ds_5$decimalLatitude)
colnames(melastom_coords_full)[2:3] = c("Longitude", "Latitude")

# exporting
write.table(melastom_coords_full, "melastom_coords_full.csv", sep=",", quote=F, row.names = F)

### processing IDonly dataset
# simplifying species name
sp_name_list = strsplit(ds_6$acceptedScientificName, split=" ")
species = c()
for (i in 1:length(sp_name_list) ){
  all_name = sp_name_list[[i]]
  species = c(species, paste(all_name[1], all_name[2], sep="_") )
}

# organizing into simple dataframe
melastom_coords_IDonly = data.frame(species, ds_6$decimalLongitude, ds_6$decimalLatitude)
colnames(melastom_coords_IDonly)[2:3] = c("Longitude", "Latitude")

# exporting
write.table(melastom_coords_IDonly, "melastom_coords_IDonly.csv", sep=",", quote=F, row.names = F)
