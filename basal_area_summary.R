### Total BA by state - will want to get a script running and let this calculate on its own
# then create a csv for it.
db <- read.csv("data/summary_table_all.csv")
db$id <- paste0("s", db$spp_code, ".img")
genera <- unique(db$genus_name)
spp.id <- db$spp_code
spp.id <- paste0("s", spp.id)
file.list <- list.files(path = "./data/", pattern = ".img", full.names = T)

# The combined spp files are not done yet, so remove them from file list
spp.id <- as.character(spp.id)
split1 <- unlist(strsplit(file.list, "a/"))
split1 <- split1[seq(from = 2, to = length(split1), by = 2)]
index <- grep(paste(spp.id, collapse = "|"), split1)
file.list[index]
list.rasters <- lapply(file.list[index], raster)

# Make a list of state shape files
state.shapes <-
  lapply(states, function(x) {
    subset(contig.usa, STATE_NAME == x)
  })
names(state.shapes) <- states


StateStats <- function(rasterfile) {
  sum.ba <- data.frame(
    x = rep(names(rasterfile), length(states)),
    y = states,
    z = NA
  )
  colnames(sum.ba) <- c("spp.id", "state", "tot.ba")
  print(names(rasterfile))
  test <- lapply(states, function(x) {})
  for (i in states) {
    sum.ba$tot.ba[sum.ba$state == i & sum.ba$spp.id == names(rasterfile)] <-
      cellStats(mask(crop(rasterfile, extent(state.shapes[[paste(i)]])),
                     state.shapes[[paste(i)]]), stat = "sum")
    print(i)
  }
  write.csv(sum.ba, paste0("data/", names(rasterfile), "states_tot_ba.csv"))
  return(sum.ba)
}

tot.ba <- lapply(raster.subset, StateStats)
tot.ba <- ldply(tot.ba)