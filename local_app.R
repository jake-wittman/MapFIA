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
library(shinyjs)

# Global data 
db <- fread("data/summary_table_all.csv") # db with name info
# Basal area data
tot.ba <- fread("data/basal_area_summary.csv")
# State shapefiles
usa <- readOGR("data/shapefiles", "states")
usa <- subset(usa, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska") # contig. usa shapefile
indv.states <- sort(as.character(unique(usa$STATE_NAME)))
# convert shapefile to format usable by ggplot
CRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
usa <- spTransform(usa, CRS)
usa@data$id <- rownames(usa@data)
gg.usa <- fortify(usa, region = "id")
gg.usa <- merge(gg.usa, usa@data, by = "id")
# Create list of all statefiles
states <- c("Contiguous USA", indv.states, "Northeast", "Mid-Atlantic", "Midwest", "Southeast", 
            "Southwest", "Mountain West", "Pacific West")



# UI ----------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(), # Include shinyjs for shinyjs commands
  
  # Application row
  titlePanel(
    fluidRow(
      column(3, div(img(height = 200, width = 200, src = "aukema_lab_logo.JPG",
                        style = "max-width: 200%; width: 100%, height: auto;"),
                    style = "text-align: center;")),
      column(9, div(br(), br(), h1("Arbor Map"),
                    style = "text-align: center;"))
      
      )
    ), # End title

# Main page chunk
  sidebarLayout(
  # sidebar inputs ----------------------------------------------------------    
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
      
      h3("Map Customization Options"),
      
      helpText("The customization options below can be changed without regenerating the map."),
      
      h4("Map Aspect Ratio"),
      
      helpText("Adjust the aspect ratio if your map looks squashed or stretched."),
      
      numericInput("aspect.ratio",
                   NULL,
                   min = 0.1,
                   value = 0.7,
                   step = 0.1),
      
      h4("Color Customization"),
       
      radioButtons("theme.customization",
                   "Theme",
                   choices = list(
                     "two color gradient" = "two.color.gradient",
                     "diverging gradient" = "div.gradient",
                     "preset palettes" = "n.color.gradient"),
                     selected = "two.color.gradient"
                   ),
      
      uiOutput("conditional.theme.customization"),
      
      width = 3
      
    ),

# main panel --------------------------------------------------------------

    mainPanel(
      
      # Tabs
      tabsetPanel(id = "tabs", type = "tabs",
                  
                  tabPanel("Instructions", 
                           
                           h3("Overview"),
                           
                           p("The purpose of this app is to provide an easy-to-use interface for anyone who is interested in visualizing data on the distribution of tree species as well as their basal area per acre in the continental US. The maps you generate can be seen under the 'Map' tab above. If you choose to map two or more species, you can also click on the bar chart tab above that shows how much of the total basal area of the tree species you mapped is made up of each species. This data comes from the U.S. Forest Service Data Archive and more specifically from:"),
                           div("Wilson, Barry Tyler; Lister, Andrew J.; Riemann, Rachel I.; Griffith, Douglas M. 2013. Live tree species basal area of the contiguous United States (2000-2009). Newtown Square, PA: USDA Forest Service, Rocky Mountain Research Station. https://doi.org/10.2737/RDS-2013-0013"),
                           
                           br(),
                           
                           h3("Instructions"),
                           
                           p("1) Select one or more tree species that you would like to map, either by their scientific name or their common name. The app will automatically fill in the other name field."),
                           
                           p(em("- If you select two or more tree species, you will have to choose whether you want to map their combined distributions or to map only where the selected species co-occur.")),
                           
                         p(strong(em("- The more species you pick, the longer it will take to produce your desired plot."))),
                         
                           p("2) Select the states you would like to include in your map."),
                         
                           p(em("- You can select all 48 contiguous states, or any combination of states. If you want to select states by a region (e.g. 'Midwest'), those options are at the bottom of the drop down menu.")),
                         
                           p(strong(em("- The fastest mapping option is 'Contiguous USA'. It will take longer to plot individual states and the more individual states you add, the longer it will take."))),
                         
                           p("3) Select the number of pixels to plot."),
                         
                           p(strong(em("- The more pixels you select, the longer it will take to map. It is strongly recommended that you start with a low number of pixels until you have identified the plot you want to produce at a higher quality."))),
                         
                           p("4) If your plot appears squished or stretched, play with the aspect ratio."),
                         
                           p("5) Adjust your color-scheme as desired."),
                         
                           p("6) If you are happy with your map and wish to download it, select the file type, DPI, and dimensions and click the 'Download' button below the map. The same can be done below the proportional bar chart if you wish to download that plot.")
                         
                         ), # end introduction tab
                  
                  tabPanel("Map", 
                           fluidRow(
                             column(8,
                                    plotOutput("map", width = "100%")
                                    ),
                           
                           column(4,
                             br(),# add a little space
                             br(),
                             helpText("The image you download will NOT appear with the same dimensions as the image displayed on this page."),
                             selectInput(
                               "file.type.map",
                               "Choose File Type for Download",
                               choices = list(
                                 "PNG" = "png",
                                 "PDF" = "pdf",
                                 "TIFF" = "tiff"
                                 )
                             ),
                             
                               numericInput(
                                 "dpi.map",
                                 "DPI",
                                 min = 150,
                                 max = 3000,
                                 step = 50,
                                 value = 300
                               ),
                               
                               numericInput(
                                 "height.map",
                                 "Height of Figure (in)",
                                 min = 0,
                                 max = NA,
                                 value = 3,
                                 step = 0.5
                               ),
                               
                               numericInput(
                                 "width.map",
                                 "Width of Figure (in)",
                                 min = 0,
                                 max = NA,
                                 value = 4.5,
                                 step = 0.5
                               ),
                               
                               downloadButton("download.map",
                                              "Download Map")
                             )
                           )
                          ),# end map tab
                           
                    tabPanel("Bar Chart",
                             fluidRow(
                               column(8,
                                  plotOutput("barchart")
                               ),
                              column(4,
                                  uiOutput("barchart.download")
                              )
                             )
                    ) # end bar chart tab
                  )# end tabset panel
    ) # end main panel
), # end side panel and main panel 


# App info ----------------------------------------------------------------
  fluidRow(
    column(3,
      p("Creator: Jacob T. Wittman, Contact: wittm094@umn.edu",
        style = "font-size: 12px"),
  
        div("All data used in this app comes from:", style = "color:black; font-size: 12px"),
  
        div(
    "Wilson, Barry Tyler; Lister, Andrew J.; Riemann, Rachel I.; Griffith, Douglas M. 2013. Live tree species basal area of the contiguous United States (2000-2009). Newtown Square, PA: USDA Forest Service, Rocky Mountain Research Station.",
          style = "color:black; font-size: 12px"
        ),
  
        a(
          "https://doi.org/10.2737/RDS-2013-0013",
          href = "https://doi.org/10.2737/RDS-2013-0013",
          style = "font-size: 12px"
        )
    )
  )# app info

    
)



# server code -------------------------------------------------------------


server <- function(input, output, session) {

# Code for scientific name and common name reactivity in input ------------

  observeEvent(input$scientific.name, {
    updateSelectizeInput(session,
                         "common.name",
                         selected = db$common[db$scientific_name %in% input$scientific.name])
  })

  observeEvent(input$common.name, {
    updateSelectizeInput(session,
                         "scientific.name",
                         selected = db$scientific_name[db$common %in% input$common.name])
  })
  
  

# selecting states by region ----------------------------------------------

  
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
                           selected = c(input$shapefiles[!input$shapefiles %in% "Midwest"], "Illinois", "Indiana", 
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

# conditional UI if spp selected > 1 -----------------------------------------------

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

# conditional map theme customization -------------------------------------------------

  output$conditional.theme.customization <- renderUI({
    # Two color gradient map
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
      # divergent gradient map
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
      # n.color gradient map
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

# barchart code -----------------------------------------------------------

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
    if ("Contiguous USA" %in% input$shapefiles) {
      regions <- indv.states
    } else {
      regions <- input$shapefiles
    }
    # Get basal area for spp & state combos
    subset.tot.ba <- tot.ba[spp_code %in% id & state %in% regions]
    # Get total basal area for selected region
    sum.tot.ba <- subset.tot.ba[, .(tot_ba = sum(tot_ba)), by = spp_code][, state := "Total"][, state_abrv := "Total"]
    # Recombine datatable
    subset.tot.ba <- rbindlist(
      list(subset.tot.ba, sum.tot.ba),
      use.names = T,
      fill = T,
      idcol = F)
    # order state list alphabetically
    selected.states <- unique(subset.tot.ba$state_abrv)
    selected.states <- selected.states[-length(selected.states)]
    selected.states <- selected.states[order(selected.states)]
    selected.states <- c(selected.states, "Total")
    subset.tot.ba$state_abrv <- factor(subset.tot.ba$state_abrv,
                                  levels = selected.states)
    
    # Percent proportion bar chart
    ggplot(subset.tot.ba, aes(x = state_abrv, y = tot_ba, fill = spp_code)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent_format()) +
      theme_bw() + 
      labs(x = "States", y = "Percentage of Total Basal Area in State") +
      scale_fill_discrete(labels = sci.name,
                          name = "Species") 
    }
  })#end barchart code

# Barchart download UI code --------------------------------------------------

  output$barchart.download <- renderUI({
    if (length(input$scientific.name) > 1 |
        length(input$common.name) > 1) {
    tagList(br(),
            br(),
            helpText("The image you download will NOT appear with the same dimensions as the image displayed on this page."),
      selectInput("file.type.barchart",
                "Choose File Type for Download",
                choices = list(
                  "PNG" = "png",
                  "PDF" = "pdf",
                  "TIFF" = "tiff"
                )),
    
    numericInput("dpi.barchart",
                 "DPI",
                 min = 150,
                 max = 3000,
                 step = 50,
                 value = 300),
    
    numericInput("height.barchart",
                 "Height of Figure (in)",
                 min = 0,
                 max = NA,
                 value = 3,
                 step = 0.5),
    
    numericInput("width.barchart",
                 "Width of Figure (in)",
                 min = 0,
                 max = NA,
                 value = 4.5,
                 step = 0.5),
    
    downloadButton("download.barchart",
                   "Download Graph")
    )}
  }) # end barchart download UI code

# map code ----------------------------------------------------------------

  ### Reactive to map button
  map <- eventReactive(input$go, {
    # Change tabset
    
    shinyjs::alert("Your map is being generated. Map generation may take several minutes, please be patient.")
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
        paths <- paste0("raster.files/", id, ".img") # get file paths
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
          color = "black") +
        ggtitle(paste("Basal area per acre of", paste(input$common.name, collapse = ", "))) 
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
          color = "black") +
        ggtitle(paste("Basal area per acre of", paste(input$common.name, collapse = ", "))) 
    }
    
    
  }) # end map generating code, responsive to go button

  # Change tabset focus on generate map button push
  observeEvent(input$go, {
    updateTabsetPanel(session, "tabs",
                      selected = "Map")
  })

# map aesthetics ----------------------------------------------------------
# Reactive customization - will not need to go through the lengthy plot generating process to change how it looks
     
  output$map <- renderPlot({
    if (input$theme.customization == "two.color.gradient") {
      # two color gradient
      map() +
        scale_fill_gradient(low = input$low,
                            high = input$high,
                            na.value = "white",
                            name = "Average Basal Area \n per Acre") +
        theme_void() +
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
        theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      
    } else { # n.gradient fill
      if (input$direction == "FALSE") { # normal direction of palette
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = 1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
        
      } else { # reverse direction
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = -1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
        
      }
    } # end n.gradient fill direction flow
    
    
   }, height = function() {
     input$aspect.ratio * session$clientData$output_map_width
   }) # end outputPlot for map

# code for map download buttons -----------------------------------------------

  
  mapInput <- function() {
    if (input$theme.customization == "two.color.gradient") {
      # two color gradient
      map() +
        scale_fill_gradient(low = input$low,
                            high = input$high,
                            na.value = "white",
                            name = "Average Basal Area \n per Acre") +
        theme_void() +
        ggtitle(paste("Basal area per acre of", paste(input$common.name, collapse = ", "))) +
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
        ggtitle(paste("Basal area per acre of", paste(input$common.name, collapse = ", "))) +
        theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
    } else { # n.gradient fill
      if (input$direction == "FALSE") { # normal direction of palette
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = 1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          ggtitle(paste("Basal area per acre of", paste(input$common.name, collapse = ", "))) +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      } else { # reverse direction
        map() +
          scale_fill_distiller(palette = input$palette,
                               na.value = "white",
                               direction = -1,
                               name = "Average Basal Area \n per Acre") +
          theme_void() +
          ggtitle(paste("Basal area per acre of", paste(input$common.name, collapse = ", "))) +
          theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
      }
    }
  }
  
  output$download.map <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "", input$scientific.name, fixed = T), ".", input$file.type.map)
      },
    content = function(file) {
      ggsave(file, 
             plot = mapInput(),
             device = input$file.type.map,
             dpi = input$dpi.map,
             width = input$width.map,
             height = input$height.map,
             units = "in")
      }
  ) # end download handler
  
  barchartInput <- function(){
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
    ggplot(subset.tot.ba, aes(x = state_abrv, y = tot_ba, fill = spp_code)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent_format()) +
      theme_bw() + 
      labs(x = "States", y = "Percentage of Total Basal Area in State") +
      scale_fill_discrete(labels = sci.name,
                          name = "Species") 
  }
  
  output$download.barchart <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "", input$scientific.name, fixed = T), ".", input$file.type.barchart)
    },
    content = function(file) {
      ggsave(file, 
             plot = barchartInput(),
             device = input$file.type.barchart,
             dpi = input$dpi.barchart,
             width = input$width.barchart,
             height = input$height.barchart,
             units = "in")
    }
  ) # end download handler
  
} #end server  
  


# Run the application
shinyApp(ui = ui, server = server)

