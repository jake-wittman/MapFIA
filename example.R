library(raster)
library(rgdal)
library(ggplot2)
library(viridis)
library(rasterVis)
library(ggthemes)


USA <- readOGR("data/shapefiles", "states") # get USA shapefile
white.oak <- raster("data/s802.img")
USA <- spTransform(USA, CRS(proj4string(white.oak))) # transform shapefile to project with raster
contig.USA <- subset(USA, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska") # contig. USA shapefile
system.time(plot(white.oak))
plot(contig.USA, add = T)

# Make the USA shapefile plotable by ggplot
USA@data$id <- rownames(USA@data)
ggUSA <- fortify(USA, region = "id")
ggUSA <- merge(ggUSA, USA@data, by = "id")
# Same but for contiguous USA
contig.USA@data$id <- rownames(contig.USA@data)
gg.contig.USA <- fortify(contig.USA, region = "id")
gg.contig.USA <- merge(gg.contig.USA, USA@data, by = "id")

ggplot(ggUSA, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = NA, color = "black")

ggplot(gg.contig.USA, aes(x = long, y = lat, group = group, fill = "none")) +
  geom_polygon(fill = NA, color = "black")


# Different way to plot..

plot <- gplot(x = white.oak) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  geom_polygon(data = gg.contig.USA, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") 
plot + scale_fill_gradientn(colors = c("#FFFFFFFF", "#FF0000FF")) + theme_map()

# Summary data
test <- white.oak
white.oak[white.oak == 0] <- NA
test[test == 0] <- NA # Converts 0 to NA
hist(test) # plots much faster this way

system.time(plot(test))
plot(contig.USA, add = T)

# Use cellStats to return a single value for analyzing raster data
# Depending on which version of the dataset I use, get different mean values...
cellStats(white.oak, stat = "mean")
cellStats(test, stat = "mean")

# I could probably iterate over each state to get summary stuff per state
