library(plyr)
library(doParallel)
library(raster)
library(rgdal)
library(ggplot2)
library(viridis)
library(rasterVis)
library(ggthemes)
library(maptools)


PlotUSA <- function(raster) {
  plot(raster)
  plot(contig.usa, add = T)
}


usa <- readOGR("data/shapefiles", "states") # get usa shapefile
white.oak <- raster("data/s802.img")
red.oak <- raster("data/s833.img")
usa <- spTransform(usa, CRS(proj4string(white.oak))) # transform shapefile to project with raster
contig.usa <- subset(usa, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska") # contig. usa shapefile
wisconsin <- subset(usa, STATE_NAME == "Wisconsin") 
mn.wi <- subset(usa, STATE_NAME == "Wisconsin" | STATE_NAME == "Minnesota")
states <- as.character(unique(contig.usa$STATE_NAME))
arizona <- subset(contig.usa, STATE_NAME == "Arizona")

#### See raster info
white.oak

### Example plots
PlotUSA(white.oak)
PlotUSA(red.oak)
PlotUSA(oak)

# Clipping to states
wi.white.oak <- mask(crop(white.oak, extent(wisconsin)), wisconsin)
plot(wi.white.oak, maxpixels = 1000)
plot(wisconsin, add = T)


mn.wi.white.oak <- mask(crop(white.oak, extent(mn.wi)), mn.wi)
plot(mn.wi.white.oak)
plot(mn.wi, add = T)

### Make the usa shapefile plotable by ggplot
# Is this really necessary?
usa@data$id <- rownames(usa@data)
ggusa <- fortify(usa, region = "id")
ggusa <- merge(ggusa, usa@data, by = "id")

# Same but for contiguous usa
contig.usa@data$id <- rownames(contig.usa@data)
gg.contig.usa <- fortify(contig.usa, region = "id")
gg.contig.usa <- merge(gg.contig.usa, usa@data, by = "id")

ggplot(ggusa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = NA, color = "black")

ggplot(gg.contig.usa, aes(x = long, y = lat, group = group, fill = "none")) +
  geom_polygon(fill = NA, color = "black")


### Plot with g(g)plot
# Default number of pixels for gplot is 50,000. Default for plot is 500,000.
# They take the same amount of [time to plot at 50,000
# At 100,000 and 200,000 pixels they're essentially the same still.
# At 5,000,000 pixels, gplot was about 2 seconds faster.
# And it doesn't like to plot 200,000,000 pixels.
system.time(plot <- gplot(x = white.oak, maxpixels = 2000) +
              geom_raster(aes(x = x, y = y, fill = value)) +
              geom_polygon(data = gg.contig.usa, aes(x = long, y = lat, group = group),
                           fill = NA, color = "black"))
system.time(plot(white.oak, maxpixels = 200000000))

plot <- gplot(x = white.oak, maxpixels = 2000) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = contig.usa, aes(x = long, y = lat, group = group),
               fill = NA, color = "black")
plot + 
  scale_fill_gradientn(colors = c("white", terrain.colors(2)),
                            name = "Basal Area") + 
  theme_map()

# Summary data


### Use cellStats to return a single value for analyzing raster data
# Depending on which version of the dataset I use, get different mean values...
# Also, doesn't match the summary value from the db
cellStats(white.oak, stat = "mean")
test <- white.oak
test[test == 0] <- NA # Converts 0 to NA
cellStats(test, stat = "mean")
hist(test) # plots much faster this way

# After reading the paper about these data (A nearest-neighbor imputation 
# approach to mapping tree species over large areas using forest inventory 
# plots and moderate resolution raster data), I think I'll be okay to use
# the mean values from these rasters with a caveat. Essentially, the mean value
# provided in the summary db are from the actual FIA plots. The mean I'm 
# getting from these rasters is the based on the values produced from the
# model that was used to generate this data. These distributions are not
# actual, but estimated based on a model. They're likely inaccurate at the
# edges of the range and for rarer species.

# I could probably iterate over each state to get summary stuff per state

### If I sum two rasters where 0 are NA, what happens?
### Also, plotting two rasters - is this the best way?
red.and.white.oak <- red.oak + white.oak
plot(red.and.white.oak)
plot(contig.usa, add = T)
plot(red.oak)

rw.overlay.oak <- overlay(red.oak, white.oak, fun = function(r1, r2) {return(r1 + r2)})
plot(rw.overlay.oak)

red.oak.NA <- red.oak
red.oak.NA[red.oak.NA == 0] <- NA
white.oak.NA <- white.oak
white.oak.NA[white.oak.NA == 0] <- NA

par(mfrow = c(2, 2))
PlotUSA(red.oak.NA)
PlotUSA(white.oak.NA)
rw.oak.NA <- red.oak.NA + white.oak.NA
PlotUSA(rw.oak.NA) # so if I sum with NA, ito only shows where they co-occur. Could be useful
rw.overlay.NA <- overlay(red.oak.NA, white.oak.NA, fun = function(r1, r2) {return(r1 + r2)})
PlotUSA(rw.overlay.NA) # same as above. 

test <- overlay(red.oak, white.oak, fun = function(r1, r2) { # this works too for plotting only co-occurance
  ifelse(r1 == 0 | r2 == 0, 0, r1 + r2)
})
PlotUSA(test)
# If I want to plot whole range of multiple species, I can't remove 0s
# If I want to show only where they co-occur, should convert 0s to NA


### Calculate mean BA by state and nationwide
test <- wi.white.oak
test[test == 0] <- NA
avg.wi.white.oak <- cellStats(test, stat = "mean")

### Genus rasters


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

### Database linking file name to species name
db <- read.csv("data/summary_table_all.csv")

# Can specify one of the plant ids and then use paste0 to get the appropriate file
input <- 802
test <- raster(paste0("data/s", input, ".img"))

### For multiple species, proportional bar charts showing basal area (total or average?)




