# Resample raster layers so the cells match

base_raster = raster("data/raw/raster_layers/bioclimatic_variables/chelsa/CHELSA_bio10_01.tif")


## Bioclimatic variables
fs_bio <- list.files(path="data/raw/raster_layers/bioclimatic_variables/chelsa", recursive = TRUE, full.names=TRUE)
stack_bio <- stack(fs_bio)
writeRaster(stack_bio, "data/processed/raster_layers/bioclimatic_variables/bioclimatic_variables", "GTiff")

## Topographic variables
fs_topo <- list.files(path="data/raw/raster_layers/topographic_variables", recursive = TRUE, full.names=TRUE)
stack_topo <- stack(fs_topo)
rasters_topo = resample(stack_topo, base_raster, "bilinear")
writeRaster(rasters_topo, "data/processed/raster_layers/topographic_variables/topographic_variables", "GTiff")


## Habitat variables
fs_earthenv <- list.files(path="data/raw/raster_layers/habitat_variables/earthenv", recursive = TRUE, full.names=TRUE)
stack_earthenv <- stack(fs_earthenv)
rasters_earthenv = resample(stack_earthenv, base_raster, "bilinear")
writeRaster(rasters_earthenv, "data/processed/raster_layers/habitat_variables/earthenv/earthenv", "GTiff")

fs_copernico <- list.files(path="data/raw/raster_layers/habitat_variables/copernico", recursive = TRUE, full.names=TRUE)
stack_copernico <- stack(fs_copernico)
rasters_copernico = resample(stack_copernico, base_raster, "bilinear")
writeRaster(rasters_copernico, "data/processed/raster_layers/habitat_variables/copernico/copernico", "GTiff")


# Extract values for occurrences
sp <- SpatialPoints(occurrences_df[,4:5])

fs <- list.files(path="data/processed/raster_layers", recursive = TRUE, full.names=TRUE)
raster_stack <- stack(fs)
tmp <- extract(raster_stack, sp)

saveRDS(tmp, "data/processed/raster_values.rds")

# Add tmp to occurrences_df
occurrences_df_bckup <- occurrences_df

for (i in 1:19){
  newcolname <- as.name(paste0("bio_", i))
  occurrences_df[[newcolname]] <- tmp[,paste0("bioclimatic_variables.",i)]
}

occurrences_df$eastness <- tmp[,paste0("topographic_variables.1")]
occurrences_df$elevation <- tmp[,paste0("topographic_variables.2")]
occurrences_df$northness <- tmp[,paste0("topographic_variables.3")]
occurrences_df$roughness <- tmp[,paste0("topographic_variables.4")]

occurrences_df$contrast <- tmp[,paste0("earthenv.1")]
occurrences_df$cv <- tmp[,paste0("earthenv.2")]
occurrences_df$homogeneity <- tmp[,paste0("earthenv.3")]
occurrences_df$maximum <- tmp[,paste0("earthenv.4")]

occurrences_df$fcover <- tmp[,paste0("copernico.1")]
occurrences_df$ndvi <- tmp[,paste0("copernico.2")]


summary(occurrences_df)

saveRDS(occurrences_df, "data/processed/occurrences_df_all_predictors.rds")


