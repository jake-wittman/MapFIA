# Load libraries
library(raster)
library(rgeos)
library(rgdal)
library(ggplot2)
library(googledrive)
library(shiny)
library(data.table)
library(scales)
library(rasterVis)
library(viridis)

# TO DO:
# Put in more options to customize resulting graphs
# For plotting multiple spp: just add them, it's faster. Also, use reclassify
# to do the cooccurrence graphs.

# Global data 
db <- fread("data/summary_table_all.csv") # db with name info
# Basal area data
tot.ba <- fread("data/basal_area_summary.csv")
# State shapefiles

usa <- readOGR("data/shapefiles", "states")
usa <- subset(usa, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska") # contig. usa shapefile
states <- sort(as.character(unique(usa$STATE_NAME)))
# conver shapefile to format usable by ggplot
CRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
usa <- spTransform(usa, CRS)
usa@data$id <- rownames(usa@data)
gg.usa <- fortify(usa, region = "id")
gg.usa <- merge(gg.usa, usa@data, by = "id")

states <- c("Contiguous USA", states, "Northeast", "Mid-Atlantic", "Midwest", "Southeast", 
            "Southwest", "Mountain West", "Pacific West")



# NOTE: Maybe need to add something that this is only for the contiguous US?
ui <- fluidPage(

  # Application title
  titlePanel("Mapping Tree Distributions"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("scientific.name",
                  "Scientific Name of Organism",
                  choices = db$scientific_name,
                  multiple = TRUE,
                  selected = NULL),

      selectizeInput("common.name",
                  "Common Name of Organism",
                  choices = db$common,
                  multiple = TRUE,
                  selected = NULL),

      selectInput("shapefiles",
                  "Geographic Area to Plot",
                  choices = states,
                  multiple = TRUE,
                  selectize = TRUE),
      
      numericInput("pixels",
                   "Number of Pixels",
                   min = 1,
                   max = 5000000,
                   value = 30000,
                   step = 10000),
      
      helpText("Plots with more pixels will take longer to generate."),
      
      uiOutput("conditional.map.options"),  
      
      actionButton("go", "Generate Map"),
      
      h3("Map Color Customization Options"),
      
      helpText("The customization options below can be changed without regenerating the map."),
      
      radioButtons("theme.customization",
                   NULL,
                   choices = list(
                     "two color gradient" = "two.color.gradient",
                     "diverging gradient" = "div.gradient",
                     "preset palettes" = "n.color.gradient"),
                     selected = "two.color.gradient"
                   ),
      
      uiOutput("conditional.theme.customization")
    ),

    mainPanel(
      plotOutput("map"),
      
      selectInput("file.type",
                "Choose File Type for Download",
                choices = list(
                  "PNG" = "png",
                  "PDF" = "pdf",
                  "TIFF" = "tiff"
                )),
      
      numericInput("dpi",
                   "DPI",
                   min = 150,
                   max = 3000,
                   step = 50,
                   value = 300),
      
      numericInput("height",
                   "Height of Figure (in)",
                   min = 0,
                   max = NA,
                   value = 3,
                   step = 0.5),
      
      numericInput("width",
                   "Width of Figure (in)",
                   min = 0,
                   max = NA,
                   value = 4.5,
                   step = 0.5),
      
      downloadButton("download.map",
                     "Download Map"),
      
      plotOutput("barchart"),
      
      downloadButton("download.graph",
                     "Download Graph")
      
    )

    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Reactive code chunk updating common name field if species selected by scientific name
  observeEvent(input$scientific.name, {
    updateSelectizeInput(session,
                         "common.name",
                         selected = db$common[db$scientific_name %in% input$scientific.name])
  })
  ### Reactive code chunk updating scientific name field if species selected by common name
  observeEvent(input$common.name, {
    updateSelectizeInput(session,
                         "scientific.name",
                         selected = db$scientific_name[db$common %in% input$common.name])
  })
  
  
  
  ### For selecting states by region
  ### Regions based on this map (http://www.pathwaystoscience.org/IBPImages/maps/smallusa.gif)
  observeEvent(input$shapefiles, {
    if ("Northeast" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Northeast"], "Connecticut", 
                                        "Maine", "Massachusetts", "New Hampshire", "New York",
                                        "Rhode Island", "Vermont"))
      
    } else if ("Mid-Atlantic" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Mid-Atlantic"], "Delaware",
                                        "Maryland", "New Jersey", "Pennsylvania", "Virginia",
                                        "West Virginia"))
      
    } else if ("Southeast" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Southeast"], "Alabama", "Arkansas", 
                                        "Florida", "Georgia", "Louisiana", "Mississippi",
                                        "North Carolina", "South Carolina", "Tennessee"))
      
    } else if ("Midwest" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Midwest"], "Illiois", "Indiana", 
                                        "Iowa", "Kansas", "Kentucky", "Michigan",
                                        "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio",
                                        "South Dakota", "Wisconsin"))
      
    } else if ("Southwest" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Southwest"], "Arizona", 
                                        "New Mexico", "Oklahoma", "Texas"))
      
    } else if ("Mountain West" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Mountain West"], "Colorado", 
                                        "Idaho", "Montana", "Nevada", "Utah", "Wyoming"))
      
    } else if("Pacific West" %in% input$shapefiles ) {
      updateSelectizeInput(session,
                           "shapefiles",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Pacific West"], "California", 
                                        "Oregon", "Washington"))
      
    }
  })
  
  ### Conditional part of side panel - show if selected species is > 1
  output$conditional.map.options <- renderUI({
    if (length(input$scientific.name) > 1 |
        length(input$common.name) > 1) {
      radioButtons(
        "dist.options",
        label = "Distribution Mapping Options",
        choices = list(
          "Map showing the combined distribution of selected species" =
            "overlay",
          "Map only the co-occurence of selected species" =
            "cooccurence"
        ),
        selected = "overlay"
      )
    }
  })
  
  ### Conditional map theme customization
  output$conditional.theme.customization <- renderUI({
    if (input$theme.customization == "two.color.gradient") {
      tagList(
        textInput("low",
                  label = "Low color",
                  value = "blue",
                  placeholder = "Low color (name or hex code)"),
      
        textInput("high",
                  label = "High color",
                  value = "orange",
                  placeholder = "High color (name or hex code)")
      )
    }
    
    else if (input$theme.customization == "div.gradient") {
      tagList(
        textInput("low",
                  label = "Low color",
                  value = "blue",
                  placeholder = "Low color (name or hex code)"),
        
        textInput("mid",
                  label = "Mid-point color",
                  value = "white",
                  placeholder = "Mid-point color (name or hex code)"),
        
        textInput("high",
                  label = "High color",
                  value = "orange",
                  placeholder = "High color (name or hex code)"),
        
        numericInput("midpoint",
                  label = "Mid-point Value",
                  value = 20,
                  min = 0,
                  max = NA)
        
      )
    }
    else if (input$theme.customization == "n.color.gradient") {
      tagList(
        selectizeInput("palette",
                       "Palette",
                       choices = list(
                         "Blues" = "Blues",
                         "Greens" = "Greens",
                         "Greys" = "Greys",
                         "Oranges" = "Oranges",
                         "Reds" = "Reds",
                         "Purples" = "Purples",
                         "Blue to green" = "BuGn",
                         "Blue to purple" = "BuPu", 
                         "Green to blue" = "GnBu",
                         "Orange to red" = "OrRd",
                         "Purple to blue" = "PuBu",
                         "Purple to blue to green" = "PuBuGn",
                         "Purple to red" = "PuRd",
                         "Red to purple" = "RdPu",
                         "Yellow to green" = "YlGn",
                         "Yellow to green to blue" = "YlGnBu",
                         "Yellow to orange to brown" = "YlOrBr",
                         "Yellow to orange to red" = "YlOrRd",
                         "Brown to blue" = "BrBG",
                         "Pink to green" = "PiYG",
                         "Purple to green" = "PRGn",
                         "Orange to purple" = "PuOR",
                         "Red to blue" = "RdBu",
                         "Red to grey" = "RdGy",
                         "Red to yellow to blue" = "RdYlBu",
                         "Red to yellow to green" = "RdYlGn",
                         "Spectral" = "Spectral",
                         "Accent" = "Accent",
                         "Dark2" = "Dark2",
                         "Paired" = "Paired",
                         "Set1" = "Set1", 
                         "Set2" = "Set2",
                         "Set3" = "Set3",
                         "Pastel1" = "Pastel1",
                         "Pastel2" = "Pastel2"
                       ),
                       selected = "GnBu"
                    ),
        checkboxInput("direction",
                      label = "Reverse direction of palette?",
                      value = FALSE)
      )
    }
  })
  
  ### Barchart code
  ### Conditional - needs >=2 "regions" selected to display
  output$barchart <- renderPlot({
    if (length(input$scientific.name) > 1 |
       length(input$common.name) > 1) {
    # Get ID for select spp
    id <- db$spp_code[db$scientific_name %in% input$scientific.name]
    # Get scientific & common names for select spp
    sci.name <- db$scientific_name[db$spp_code %in% id]
    com.name <- db$common[db$spp_code %in% id]
    # Get region for entered states
    regions <- input$shapefiles
    # Get basal area for spp & state combos
    subset.tot.ba <- tot.ba[spp_code %in% id & state %in% regions]
    # Get total basal area for selected region
    sum.tot.ba <- subset.tot.ba[, .(tot_ba = sum(tot_ba)), by = spp_code][, state := "Total"]
    # Recombine datatable
    subset.tot.ba <- rbindlist(
      list(subset.tot.ba, sum.tot.ba),
      use.names = T,
      fill = T,
      idcol = F)
    
    # Percent proportion bar chart
    ggplot(subset.tot.ba, aes(x = state, y = tot_ba, fill = spp_code)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent_format()) +
      theme_bw() + 
      labs(x = "States", y = "Percentage of Total Basal Area in State") +
      scale_fill_discrete(labels = sci.name,
                          name = "Species") 
    }
  })#end barchart code

  ### Map code
  ### Reactive to map button
  map <- eventReactive(input$go, {
    # Get ID code for each spp
    id <- db$spp_code[db$scientific_name %in% input$scientific.name]
    # Get scientific & common names for select spp
    sci.name <- db$scientific_name[db$spp_code %in% id]
    com.name <- db$common[db$spp_code %in% id]
    # Get region for entered states
    regions <- input$shapefiles
    if (length(id) > 1) {
      # For plotting more than 1 spp
      if (input$dist.options == "overlay") {
        # for plotting overlay
        paths <- paste0("raster.files/", id, ".img")
        spp.raster <- reclassify(sum(stack(paths)), c(-0.001, 0.001, NA))
      } else {
        #for plotting co-occurence
        paths <- paste0("raster.files/", id, ".img")
        paths <- lapply(paths, raster)
        paths <- lapply(paths, reclassify, c(-0.001, 0.001, NA))
        spp.raster <- sum(stack(paths))
      } # end multiple spp chunk
      
    } else {
      spp.raster <- reclassify(raster(paste0("raster.files/", id, ".img")), c(-0.001, 0.001, NA))
    }
    if ("Contiguous USA" %in% regions) {
      # If USA is selected, plot whole US
      # Make plot
      # Need to reclassify 0s to NA so plot looks okay. Doesn't affect pixel values
      plot <-
        gplot(x = spp.raster,
              maxpixels = input$pixels) +
        geom_raster(aes(x = x, y = y, fill = value)) +
        geom_polygon(
          data = gg.usa,
          aes(x = long, y = lat, group = group),
          fill = "transparent",
          color = "black"
        )
    } else {
      # plot just selected state shapefiles
      # Crop raster to extent of selected polygons
      sub.states <- subset(usa, STATE_NAME %in% regions)
      spp.raster <-
        mask(crop(spp.raster, extent(sub.states)), sub.states)
      sub.states@data$id <- rownames(sub.states@data)
      sub.states <- fortify(sub.states, region = "id")
      
      # Produce plot
      plot <- gplot(x = spp.raster, maxpixels = input$pixels) +
        geom_raster(aes(x = x, y = y, fill = value)) +
        geom_polygon(
          data = sub.states,
          aes(x = long, y = lat, group = group),
          fill = NA,
          color = "black"
        )
    }
    
    
  }) # end map generating code, responsive to go button
        
     
  output$map <- renderPlot({
    # This part here v needs to be reactive to changing plot aesthetics
    # but should not remake plot
    if (input$theme.customization == "two.color.gradient") {
      # two color gradient
      map() +
        scale_fill_gradient(low = input$low,
                            high = input$high,
                            na.value = "white",
                            name = "Average Basal Area \n per Acre") +
        theme_void() +
        ggtitle(paste("Basal area per pixel of", input$common.name)) +
        theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      
    } else if (input$theme.customization == "div.gradient") { 
      #diverging gradient fill
      map() +
        scale_fill_gradient2(
          low = input$low,
          mid = input$mid,
          high = input$high,
          midpoint = input$midpoint,
          na.value = "white",
          name = "Average Basal Area \n per Acre") +
        theme_void() +
        ggtitle(paste("Basal area per pixel of", input$common.name)) +
        theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      
    } else { # n.gradient fill
      if (input$direction == "FALSE") { # normal direction of palette
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = 1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          ggtitle(paste("Basal area per pixel of", input$common.name)) +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
        
      } else { # reverse direction
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = -1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          ggtitle(paste("Basal area per pixel of", input$common.name)) +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      }
    } # end n.gradient fill direction flow
    
   }) # end outputPlot for map
  
  plotInput <- function() {
    if (input$theme.customization == "two.color.gradient") {
      # two color gradient
      map() +
        scale_fill_gradient(low = input$low,
                            high = input$high,
                            na.value = "white",
                            name = "Average Basal Area \n per Acre") +
        theme_void() +
        ggtitle(paste("Basal area per pixel of", input$common.name)) +
        theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
    } else if (input$theme.customization == "div.gradient") { 
      #diverging gradient fill
      map() +
        scale_fill_gradient2(
          low = input$low,
          mid = input$mid,
          high = input$high,
          midpoint = input$midpoint,
          na.value = "white",
          name = "Average Basal Area \n per Acre") +
        theme_void() +
        ggtitle(paste("Basal area per pixel of", input$common.name)) +
        theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
    } else { # n.gradient fill
      if (input$direction == "FALSE") { # normal direction of palette
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = 1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          ggtitle(paste("Basal area per pixel of", input$common.name)) +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      } else { # reverse direction
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = -1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          ggtitle(paste("Basal area per pixel of", input$common.name)) +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      }
    }
  }
  
  output$download.map <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "", input$scientific.name, fixed = T), ".", input$file.type)
      },
    content = function(file) {
      ggsave(file, 
             plot = plotInput(),
             device = input$file.type,
             dpi = input$dpi,
             width = input$width,
             height = input$height,
             units = "in")
      }
  ) # end download handler
  
  
  
} #end server  
  


# Run the application
shinyApp(ui = ui, server = server)

