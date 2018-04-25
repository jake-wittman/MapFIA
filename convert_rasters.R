library(raster)
library(rgdal)


# Takes rasters, turns 0 values to NA and then writes new raster to disk
file.list <- list.files(path = "./raster.files/", pattern = ".img", full.names = T)
id <- unlist(strsplit(file.list, "es/"))
id <- id[seq(from = 2, to = length(id), by = 2)]
file.list <- unlist(file.list)
for (i in 1:length(id)) {
  print(file.list[i])
  temp.path <- file.list[i]
  temp.raster <- reclassify(raster(temp.path), c(-0.001, 0.001, NA))
  writeRaster(temp.raster, paste0("NArasters/", id[i]), overwrite = F, options = "COMPRESSED=YES")
  rm(temp.raster)
}
