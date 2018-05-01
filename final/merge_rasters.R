library(raster)
library(rgdal)
library(beepr)


# Get db with species codes
# As a note, I manually removed id 299, 915, 998, and 999 because
# they're for dead trees
db <- read.csv("data/summary_table_all.csv")
db$id <- paste0("s", db$spp_code, ".img")
genera <- unique(db$genus_name)
id <- db$id
file.list <- list.files(path = "./data/", pattern = ".img", full.names = T)

split1 <- unlist(strsplit(file.list, "a/"))
split1 <- split1[seq(from = 2, to = length(split1), by = 2)]

# Start here
tree.id <- db$id[db$genus_name == "Ulmus" & db$spp_name != "spp." ]
tree.id <- as.character(tree.id)

index <- grep(paste(tree.id, collapse = "|"), split1)
file.list[index]
rasters <- lapply(file.list[index], raster)


for (i in 1:length(rasters)) {
  if (i == 1){
    combined <- rasters[[i]]
  } else {
    combined <- combined + rasters[[i]]
  }
}
beep(6)
plot(combined)


# CHECK THAT FILE NAME IS RIGHT!!!!!!!!!!!!!!!!!!!!!!!!!
# STOP AND CHECK RIGHT NOW!!!!!!!!!!!!!!!
writeRaster(combined, "data/s970.img", overwrite = T, options = "COMPRESSED=YES")
# The file sizes are too big right now, so I'll come back to this.

# Completed genera
# Abies
# Chamaecyparis
# Juniperus
# Larix
# Picea
# Pinus
# Taxodium
# Thuja
# Tsuga
# Acer
# Aesculus
# Alnus
# Amelanchier
# Arbutus
# Betula
# Carya
# Castanea
# Catalpa
# Celtis
# Crataegus
# Diospyros
# Fraxinus
# Gleditsia
# Halesia
# Juglans
# Magnolia
# Malus
# Morus
# Nyssa
# Populus
# Prosopis
# Prunus
# Quercus
# Salix
# Tilia
# Ulmus







