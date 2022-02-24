library(raster)
library(rgdal)
#library(gdalUtils)

r1 = raster("data/raw/raster_layers/bioclimatic_variables/chelsa/CHELSA_bio10_01.tif")


r2 = raster("data/raw/topographic_variables/continuous/eastness_1KMmd_GMTEDmd.tif")

r.new = resample(r2, r1, "bilinear")

writeRaster(r.new, "eastness_1KMmd_GMTEDmd", "GTiff")

r2 = raster("data/raw/topographic_variables/continuous/elevation_1KMmd_GMTEDmd.tif")

r.new = resample(r2, r1, "bilinear")

writeRaster(r.new, "elevation_1KMmd_GMTEDmd", "GTiff")

r2 = raster("data/raw/topographic_variables/continuous/northness_1KMmd_GMTEDmd.tif")

r.new = resample(r2, r1, "bilinear")

writeRaster(r.new, "northness_1KMmd_GMTEDmd", "GTiff")

r2 = raster("data/raw/topographic_variables/continuous/roughness_1KMmd_GMTEDmd.tif")

r.new = resample(r2, r1, "bilinear")

writeRaster(r.new, "roughness_1KMmd_GMTEDmd", "GTiff")


