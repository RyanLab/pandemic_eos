
# Pandemic EOS Dashboard
#addResourcePath("www", file.path(getwd(), "www"))

# setwd("R:/Ryan_Lab/Jakob_M/Projects/PAND_EOS_Dash")

# Load in packages
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(tidyverse) 
library(maps)
library(mapproj)
library(mapdata)
library(ggthemes)
library(maps)
library(ggplot2)
library(viridis)
library(viridisLite)
library(gridExtra)
library(ggspatial)
library(tigris)
library(ggpubr)
library(tidyterra)
library(paletteer)
library(classInt)
library(shinyWidgets)
library(rsconnect)
library(here)
library(treemap)
library(plotly)

# Load in global map and set projection to match points
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% st_transform(4326)

# Use column geounit for joining purposes if needed
# print(world$geounit)

# Load in locations of news articles
locations <- read_csv(here("EOS_locations.csv"), locale = locale(encoding = "UTF-8")) %>%
  mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))

# locations <- read_csv("EOS_locations.csv", locale = locale(encoding = "UTF-8")) %>%
#   mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))

locations <- locations %>% 
  filter(!is.na(Lat), !is.na(Long)) %>% 
  mutate(
    Long = as.numeric(gsub("[^0-9.-]", "", iconv(Long, "UTF-8", "ASCII", sub = ""))),
    Lat = as.numeric(gsub("[^0-9.-]", "", iconv(Lat, "UTF-8", "ASCII", sub = "")))
    # Location = iconv(Location, "UTF-8", "ASCII", sub = "")
    )

locations <- locations %>% 
  mutate(
    lat_j = jitter(Lat, amount = 0.005),
    long_j = jitter(Long, amount = 0.005)
  )
  
locations %>% 
  filter(is.na(lat_j) | is.na(long_j) | !is.finite(lat_j) | !is.finite(long_j))

plot_data <- locations %>%
  filter(!is.na(lat_j), !is.na(long_j))

plot_data <- plot_data %>% 
  rename("date" = "Publication Date",
         "location" = "Location",
         "disease" = "Disease (target)",
         "disrep" = "DiseaseReport",
         "distype" = "Disease Type (airborne, waterborne, vectorborne)",
         "obj" = "Study Objective",
         "scale" = "Study Scale",
         "scalerep" = "ScaleReport",
         "sat" = "Satellites",
         "satrep" = "SatReport",
         "sensrep" = "SensReport",
         "resrep" = "ResReport",
         "georep" = "GeophysReport",
         "terrep" = "TerraReport",
         "res" = "Spatial resolution",
         "rescat" = "ResCat",
         "genvar" = "Geophysical Environmental Variables",
         "title" = "Title",
         "tervar" = "Terrestrial variables included?",
         "scale2" = "NewScale")

plot_data <- plot_data %>% 
  mutate(unique_id = row_number())

###### PICTURE PIPELINE CHANGE VARIABLE FOR EACH REPORTING FACTOR
var <- "disease"

counts <- plot_data %>% 
  # expand_var(var) %>% 
  dplyr::count(date, disease) %>% 
  arrange(desc(n))

# chart_data <- plot_data %>%
#   dplyr::rename(selected_var = all_of(var)) %>%
#   mutate(selected_var = ifelse(
#     is.na(selected_var) | trimws (as.character(selected_var)) == "",
#     "Unknown", as.character(selected_var)
#   )) %>%
#   filter(!is.na(date)) %>%
#   dplyr::count(date, selected_var)

jpeg("treemap_disease.jpeg", width = 1200, height = 800, quality = 100, res = 150)
treemap(counts,
        index = "disease",
        vSize = "n",
        type = "index",
        palette = plasma(nrow(counts)),
        title = "Diseases")
dev.off()

ggsave(
  filename = "piechart_disease.jpeg",
  plot = ggplot(counts, aes(x = "", y = n, fill = disease)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = plasma(length(unique(counts$disease))),
                      name = "disease") +
    labs(title = "Type of Disease") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right"),
  width = 10, height = 8, dpi = 300, device = "jpeg"
)

ggsave(
  filename = "barchart_disease.jpeg",
  plot = ggplot(counts, aes(x = date, y = n, fill = disease)) +
    geom_bar(stat = "identity", color = "white", linewidth = 0.2) +
    scale_fill_manual(values = plasma(length(unique(counts$disease))),
                      name = "Disease") +
    scale_x_continuous(
      breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)
    ) +
    labs(title = "Publications Per Year by Disease",
         x = "Year", y = "Number of Publications") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.position = "right"),
  width = 12, height = 8, dpi = 300, device = "jpeg"
)

#####

expand_var <- function(data, var) {
  data %>%
    mutate(selected_var = as.character(.data[[var]])) %>%
    mutate(selected_var = ifelse(
      is.na(selected_var) | trimws(selected_var) == "" | selected_var == "NA",
      "Unknown", selected_var
    )) %>%
    mutate(split_var = strsplit(selected_var, ",\\s*")) %>%
    tidyr::unnest(split_var) %>%
    mutate(selected_var = trimws(split_var)) %>%
    mutate(selected_var = ifelse(
      is.na(selected_var) | trimws(selected_var) == "",
      "Unknown", selected_var
    )) %>%
    select(-split_var)
}

ui <- navbarPage("PAND_EOS Articles Catalog",
                 
                 header = tags$head(
                       tags$style(HTML("
                           #Make the sidebars look cleaner
                           .sidebar-header {
                             color: #2c3e50;
                             font-weight: bold;
                             margin-top: 20px;
                           }

                           # Style the main instructional text
                           .instruction_text {
                           font-size: 16px;
                           line-height: 1.6;
                           color: #444444;
                           background-color: #f9f9f9;
                           padding: 15px;
                           border-left: 5px solid #007bc2;
                           border-radius: 4px;
                           }

                           #Title text
                           .title-header {
                           text-align: center;
                           padding: 20px;
                           color: #2c3e50;
                           width: 100%;
                           }
                           
                           #hover_info {
                            margin-bottom: 10px;
                            padding: 10px 15px;
                            background-color: #f0f4f8;
                            border-left: 5px solid #007bc2;
                            border-radius: 4px;
                            font-size: 15px;
                            color: #2c3e50;
                            min-height: 40px;
                            }
                           "))
                                  ),
                 
                 tabPanel(
                   "Home Page",
                   titlePanel("Home"),
                   sidebarLayout(
                     sidebarPanel(
                       h4("What is a pandemic?"),
                       div(
                         tags$b("What is a pandemic?"),
                         tags$ul(
                           tags$li("A pandemic is a disease outbreak that has spread beyond its native geographic region and is difficult to contain. Pandemics are caused by “pathogens”, which are microorganisms (or germs) that are spread through different ways to humans and which cause disease in humans."),
                           tags$li("There are several different ways that rapid transmission can occur leading to a pandemic, including:"),
                           tags$ul(
                             tags$li("Airborne transmission (pathogens flying through the air on particles which are breathed by humans)"),
                             tags$li("Waterborne transmission (pathogens moving through water which is consumed by humans)"),
                             tags$li("Vectorborne transmission (pathogens being carried by insect vectors and transmitted to people through biting)"),
                             tags$li("Foodborne transmission (pathogens moving through food and being consumed by humans)")
                           ),
                           tags$li("Very often, disease outbreaks are directly connected to environmental changes which expose people to pathogens that they have never encountered before.")
                         ),
                         tags$b("What is a satellite?"),
                         tags$ul(
                           tags$li("An artificial object moving in Earth’s gravitational field that may contain different types of instruments used for imaging (taking pictures) of the Earth’s surface."),
                           tags$li("Many satellites will be sent into space with the specific goal of taking pictures of a certain type of geographic feature or phenomenon."),
                           tags$li("This process of taking pictures of the Earth’s surface for analysis of environmental changes and processes is called “remote sensing”")
                         ),
                         tags$b("What is a sensor?"),
                         tags$ul(
                           tags$li("Sensors are instruments designed to be hosted by the satellite and take pictures of the Earth’s surface, often using different “bands” which correspond to varying spectra of light."),
                           tags$li("These different “bands” of light are naturally emitted by reflections of sunlight off of the Earth’s surface, and a computer inside the sensor can decipher what different levels of light translate to in a quantitative value which is then used to represent the level of each type of radiation."),
                           tags$li("Example: Red, Green, Blue, and Natural Infrared bands (among others) can be used to determine the level of vegetation growing in a specific area.")
                         ),
                         tags$b("What is a spatial resolution?"),
                         tags$ul(
                           tags$li("Sensors gather this information at a specific “spatial resolution”"),
                           tags$li("Spatial resolution is the smallest area that the satellite is creating images at, and is typically described with dimensions such as 100 x 100 meters, which indicate the size of the pixel generated with the corresponding data."),
                           tags$li("The pixel data is then transmitted to a receiver computer on Earth that compiles the information and generates images with all of the pixel data arranged together.")
                         )
                       )
                     ),
                     
                   mainPanel(
                     h4("Connecting the Dots"),
                     div(
                       tags$b("How are satellites used for predicting pandemics?"),
                       tags$ul(
                         tags$li("Since so many pandemics have transmission pathways that are caused by environmental processes, we know that examining environmental changes can be an impactful way to determine the risk of pandemics"),
                         tags$li("By using remote sensing tools to look at different natural and anthropogenic environmental factors that play a role in disease transmission, we can understand the relative risk of pandemics in different areas around the world."),
                         tags$ul(
                           tags$li("Example: if deforestation is detected via remote sensing in some areas, we know that populations moving into those newly deforested areas are more vulnerable to vectorborne and zoonotic disease outbreaks because of proximity to animals and insect vectors.")
                         ),
                         tags$li("Remote sensing as a tool for pandemic preparedness has rapidly evolved since the global COVID-19 pandemic, with many research teams using it as a tool to study how environmental variables may have contributed to the spread of COVID-19"),
                         tags$li("Oftentimes, remote sensing data is openly accessible for public use, meaning that anybody who is able to download and work with the data is able to use it to answer their research questions"),
                         tags$li("This is particularly important in the context of global pandemic preparedness as it allows global researchers to approach this common goal from different angles")
                       )
                     ),
                     h4("Future Developments"),
                     div(
                       tags$b("What is the purpose of this study?"),
                       tags$ul(
                         tags$li("The primary goal of this literature review is to examine the different studies that have been conducted related to pandemic preparedness using remote sensing systems and summarize the findings so that eventually a standardized pipeline for this process can be created"),
                         tags$li("With that in mind, this review analyzes the articles through eight different lenses of replicability:"),
                         tags$ul(
                           tags$li("Disease reported"),
                           tags$li("Type of disease"),
                           tags$li("Study scale"),
                           tags$li("Satellite used"),
                           tags$li("Remote sensor used"),
                           tags$li("Spatial resolution used"),
                           tags$li("Geophysical environmental variables included"),
                           tags$li("Terrestrial variables included")
                         ),
                         tags$li("It is our hope that by compiling these resources, we will be able to find common ground between them that can be used to develop a standardized pipeline of pandemic prediction using Earth observation systems (EOS)")
                     )
                   )
                   )
                   )
                 ),
                 
                 tabPanel(
                   "Articles Map",
                   titlePanel("Global Articles Map"),
                   # fluidRow(
                   #   column(
                   #     width = 8,
                   #     offset = 4,
                   #     div(
                   #       style = "margin-top: 15px; margin-bottom: 15px; padding: 12px 20px;
                   #                background-color: #f0f4f8; border-left: 5px solid #007bc2;
                   #                border-radius: 4px; font-size: 15px; color: #2c3e50;",
                   #       textOutput("hover_info")
                   #     )
                   #   )
                   # ), 
                         sidebarLayout(
                           sidebarPanel(
                             # p("Select a location to zoom the camera to that region"),
                             # 
                             # selectInput("region_select", "Zoom to region", choices = NULL),
                             # selectInput("country_select", "Zoom to country", choices = NULL),
                             # 
                             # hr(),
                             
                             p("Select a variable to view a color-coded version of the map"),

                             radioButtons(
                               inputId = "color_by",
                               label = "Color Markers By:",
                               choices = c(
                                 "Disease" = "disease",
                                 "Disease Type" = "distype",
                                 "Study Scale" = "scale2",
                                 "Satellite Used" = "NewSat",
                                 "Sensor Used" = "Sensor",
                                 "Spatial Resolution" = "rescat",
                                 "Year" = "date"
                               ),
                               selected = "none"
                             ),

                             p("Use the map to click on the markers for more details"),

                             hr(),

                             uiOutput("hover_checklist"),
                             
                             hr(),
                             
                             uiOutput("total_stats")
                           ),
                           
                           
                  mainPanel(
                    leafletOutput("map", height = 700),
                    br(),
                    uiOutput("click_info")
                    # fluidRow(
                    #   style = "margin-top: 50px;",
                    #   column(
                    #     width = 10,
                    #     offset = 1,
                    #     plotOutput("binary_heatmap", height = 800)
                    #   )
                    # )
                    # br(),
                    # uiOutput("click_info")
                   )
                         ),
                   
                           # fluidRow(
                           #   style = "margin-top: 50px;",
                           #   column(
                           #     width = 8, 
                           #     offset = 2, 
                           #     plotOutput("chart_year", height = 600))
                           # ),
                   
                   #       #   fluidRow(
                   #       #     column(6, plotOutput("heatmap_disease", height = 600)),
                   #       #     column(6, plotOutput("heatmap_species", height = 600))
                   #       #   ),
                   
                   # br(),
                   # div(
                   #   style = "text-align: center; padding: 20px; background-color: #f5f5f5; border-radius: 5px;",
                   #   h3(textOutput("hover_info"), style = "margin: 0; font-size: 28px; font-weight: bold;"
                   #   )
                   # ),
                   # br(),
                   
                 ),
                  
                 tabPanel(
                   "Charts and Graphs",
                   titlePanel("Charts and Graphs"),
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       p("Select a variable to view a treemap and pie chart of variable occurrence"),
                       
                       radioButtons(
                         inputId = "treemap_var",
                         label = "Display Treemap and Pie Chart By:",
                         choices = c(
                           "Disease" = "disease",
                           "Disease Type" = "distype",
                           "Study Scale" = "scale2",
                           "Satellite Used" = "NewSat",
                           "Sensor Used" = "Sensor",
                           "Spatial Resolution" = "rescat",
                           "Geophysical Variables" = "genvar",
                           "Terrestrial Variables" = "tervar",
                           "Year" = "date"
                         ),
                         selected = "NewSat"
                       ),
                       
                       h4("Treemap/Pie Chart"),
                       div(
                         tags$p("Each figure to the right shows the frequency that different factors were reported over the scope of the literature review. Items that appear in larger boxes (treemap) or wedges (pie chart) are mentioned more often in the literature review than others.")
                       ),
                       h4("Stacked Bar Chart"),
                       div(
                         tags$p("Each column shows the total number of publications cited for each year in the range of the study period. Within each column, the different colors correspond to the number of times a specific item within each variable was mentioned, to show the difference in characteristics of the criteria seen over time.")
                       ),
                       h4("Table"),
                       div(
                         tags$p("Gridded display of the criteria reported for each article in the literature review. Dark shade indicates that the criterion was reported while lighter shade indicates it was not. Scroll to the bottom of the table to see the criteria listed and match them with the article names on the left side of the table to determine presence or not.")
                       )
                     
                       
                       # p("Use the map to click on the markers for more details"),
                       # 
                       # hr(),
                       # 
                       # uiOutput("hover_checklist"),
                       # 
                       # hr(),
                       # 
                       # uiOutput("total_stats"),
                       # 
                       # hr(),
                       # 
                       # uiOutput("click_info")
                     ),
                     
                   mainPanel(
                     fluidRow(
                       style = "margin-top: 50px;",
                       column(
                         width = 5,
                         offset = 1,
                         plotOutput("tree_var", height = 600)),
                       column(
                         width = 5,
                         offset = 1,
                         plotOutput("pie_var", height = 400)),
                     )
                     # mainPanel(
                     #   fluidRow(
                     #     style = "margin-top: 50px;",
                     #     column(
                     #       width = 5,
                     #       offset = 1,
                     #       plotOutput("tree_sat", height = 600)),
                     #     column(
                     #       width = 5,
                     #       offset = 1,
                     #       plotOutput("tree_sens", height = 600)),
                     #   )
                       # fluidRow(
                       #   style = "margin-top: 50px;",
                       #   column(
                       #     width = 8, 
                       #     offset = 2, 
                       #     plotOutput("chart_year", height = 600))
                       # )
                     )
                   ),
                   
                   fluidRow(
                     style = "margin-top: 50px;",
                     column(
                       width = 10, 
                       offset = 1, 
                       plotOutput("chart_year", height = 600))
                   ),
                   fluidRow(
                     style = "margin-top: 50px;",
                     column(
                       width = 10, 
                       offset = 1, 
                       # plotOutput("binary_heatmap", height = 800)
                       div(
                         style = "overflow-y: scroll; height: 600px; border: 1px solid #ddd; border-radius: 4px;",
                         plotOutput("binary_heatmap", height = paste0(nrow(plot_data) * 25, "px"))
                       )
                       # plotlyOutput("binary_heatmap", height = "700px")
                       )
                   )
                 )
                   
                 )
                 
                 

server <- function(input, output, session) {
  
  # hover_data <- reactiveVal("Hover over a circle marker")
  
  output$map <- renderLeaflet({
    color_by <- input$color_by
    n <- nrow(plot_data)
    
    map <- leaflet(options = leafletOptions(preferCanvas = FALSE)) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2)
    
    if (is.null(color_by) || color_by == "none" || n == 0) {
      return(
        map %>% addCircleMarkers(
          data = plot_data,
          lng = ~long_j,
          lat = ~lat_j,
          radius = 4,
          color = ~ifelse(SatMult == "Yes" | SensMult == "Yes", "white", "black"),
          weight = ~ifelse(SatMult == "Yes" | SensMult == "Yes", 2, 1),
          fillColor = "black", 
          fillOpacity = 0.8,
          stroke = TRUE,
          options = pathOptions(interactive = TRUE),
          layerId = ~as.character(unique_id),
          # layerId = ~paste0(lat_j, "_", long_j),
          label = lapply(paste0(
            "<b>Location:</b> ", plot_data$location, "<br>",
            "<b>Year:</b> ", plot_data$date, "<br>",
            "<b>Disease:</b> ", plot_data$disease, "<br>",
            "<b>Disease Type:</b>", plot_data$distype, "<br>",
            "<b>Satellites Used:</b> ", plot_data$sat),
            HTML)
        )
      )
    }
    
    values <- plot_data[[color_by]]
    values[is.na(values) | trimws(values) == ""] <- "Unknown"

    unique_vals <- unique(values)

    pal <- colorFactor(
      palette = plasma(length(unique_vals)),
      domain = unique_vals
    )

    legend_title <- switch(color_by,
                           "disease" = "Disease",
                           "distype" = "Disease Type",
                           "scale2" = "Study Scale",
                           "Satellite Used" = "NewSat",
                           "Sensor Used" = "Sensor")
    
    # values <- plot_data[[color_by]]
    # values[is.na(values) | trimws(as.character(values)) == ""] <- NA
    # 
    # if (color_by == "date") {
    #   pal <- colorNumeric(palette = plasma(100), domain = values, na.color = "grey")
    #   legend_title <- "Year"
    # } else {
    #   values <- as.character(values)
    #   values[is.na(values)] <- "Unknown"
    #   unique_vals <- unique(values)
    #   pal <- colorFactor(palette = plasma(length(unique_vals)), domain = unique_vals)
    #   legend_title <- switch(color_by,
    #                          "disease" = "Disease", "distype" = "Disease Type",
    #                          "scale2" = "Study Scale", "NewSat" = "satellite Used", "Sensor" = "Sensor Used"
    #                          )
    # }
    
    map %>%
      addCircleMarkers(
        data = plot_data,
        lng = ~long_j,
        lat = ~lat_j,
        radius = 4,
        color = ~ifelse(SatMult == "Yes" | SensMult == "Yes", "black", "black"),
        weight = ~ifelse(SatMult == "Yes" | SensMult == "Yes", 3, 1),
        fillColor = ~pal(values), 
        fillOpacity = 0.8,
        stroke = TRUE,
        options = pathOptions(interactive = TRUE),
        layerId = ~as.character(unique_id),
        # layerId = ~paste0(lat_j, "_", long_j),
        label = lapply(paste0(
          "<b>Location:</b> ", plot_data$location, "<br>",
          "<b>Year:</b> ", plot_data$date, "<br>",
          "<b>Disease:</b> ", plot_data$disease, "<br>",
          "<b>Disease Type:</b>", plot_data$distype, "<br>",
          "<b>Satellites Used:</b> ", plot_data$sat),
          HTML)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values,
        title = legend_title,
        opacity = 1
      )
  })
  
  hover_data <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_mouseover, {
    event <- input$map_marker_mouseover
    print(paste("SHAPE mouseover fired, id:", event$id))
    # Establishing source of hover text data
    if(!is.null(event$id)) {
      check_data <- plot_data[plot_data$unique_id == as.integer(event$id), ]
      print(paste("rows matched:", nrow(check_data)))
      if(nrow(check_data) > 0) {
        
        # plot_data <- plot_data[1, ]
        
        # Find the currently selected column
        # col <- selected_column()
        
        # Get the value from that column
        # check_value <- check_data[[col]]
        
        # Calculate number months
        # months <- ifelse(is.na(scenario_value), 0, round(scenario_value, 1))
        
        # Label
        # label_text <- paste0("Checklist: ", 
        #                      "Disease? ", check_data$disrep, 
        #                      " Scale? ", check_data$scalerep, 
        #                      " Satellite? ", check_data$satrep, 
        #                      " Sensor? ", check_data$sensrep,
        #                      " Spatial Resolution? ", check_data$resrep,
        #                      " Geophysical Variables? ", check_data$georep,
        #                      " Terrestrial Variables? ", check_data$terrep)
        # print(paste("label_text:", label_text))
        hover_data(as.data.frame(check_data))
      }
    }
  })

  # output$hover_checklist <- renderUI({
  #   data <- hover_data()
  #   if (is.null(data) || identical(data, "Hover over a circle marker")) {
  #     return(p("Hover over a marker to see its checklist.", style = "color: #888;"))
  #   }
  #   div(
  #     style = "margin-top: 10px;",
  #     h4("Reporting Checklist", style = "color: #2c3e50; font-weight: bold;"),
  #     tags$table(
  #       style = "width: 100%; font-size: 14px;",
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Disease Reported:"),
  #         tags$td(style = "padding: 4px;", data$disrep)
  #       ),
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Scale Reported:"),
  #         tags$td(style = "padding: 4px;", data$scalerep)
  #       ),
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Satellite Reported:"),
  #         tags$td(style = "padding: 4px;", data$satrep)
  #       ),
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Sensor Reported:"),
  #         tags$td(style = "padding: 4px;", data$sensrep)
  #       ),
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Spatial Resolution Reported:"),
  #         tags$td(style = "padding: 4px;", data$resrep)
  #       ),
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Geophysical Variables Reported:"),
  #         tags$td(style = "padding: 4px;", data$georep)
  #       ),
  #       tags$tr(
  #         tags$td(style = "font-weight: bold; padding: 4px;", "Terrestrial Variables Reported:"),
  #         tags$td(style = "padding: 4px;", data$terrep)
  #       )
  #     ),
  #     {
  #       fields <- c(data$disrep, data$scalerep, data$satrep, data$sensrep,
  #                   data$resrep, data$georep, data$terrep)
  #       yes_count <- sum(tolower(trimws(fields)) == "yes", na.rm = TRUE)
  #       total <- length(fields)
  #       h2(
  #         paste0(yes_count, " / ", total, " criteria reported"),
  #         style = "text-align: center; color: 007bc2; font-weight: bold; margin-top: 15px;"
  #       )
  #     }
  #   )
  # })
    
  output$hover_checklist <- renderUI({
    data <- hover_data()
    no_data <- is.null(data) || !is.data.frame(data) || nrow(data) == 0
    
    fields <- list(
      list(label = "Disease Reported: ", col = "disrep"),
      list(label = "Scale Reported: ", col = "scalerep"),
      list(label = "Satellite Reported: ", col = "satrep"),
      list(label = "Sensor Reported: ", col = "sensrep"),
      list(label = "Spatial Resolution Reported: ", col = "resrep"),
      list(label = "Geophysical Variables Reported: ", col = "georep"),
      list(label = "Terrestrial Variables Reported: ", col = "terrep")
    )
    
    rows <- lapply(fields, function(f) {
      value <- if (no_data) "-" else as.character(data[[f$col]])
      tags$tr(
        tags$td(style = "font-weight: bold; padding: 4px;", f$label),
        tags$td(style = "padding: 4px;", value)
      )
    })
    
    yes_count <- if (no_data) 0 else {
      sum(sapply(fields, function(f) {
        tolower(trimws(as.character(data[[f$col]]))) == "yes"
    }), na.rm = TRUE)
    }
    
    div(
      style = "margin-top: 10px;",
      h4("Reporting Checklist", style = "color: #2c3e50; font-weight: bold;"),
      p(if (no_data) "Hover over a marker to see its checklist" else "\u00a0",
        style = "color: #888; font-size: 12px; margin: 0;"),
      tags$table(style = "width: 100%; font-size: 14px;", do.call(tagList, rows)),
      h2(
        paste0(yes_count, " / ", length(fields), " criteria reported"),
        style = "text-align: center; color: #007bc2; font-weight: bold, margin-top: 15px;"
      )
    )
  })
  
  output$total_stats <- renderUI({
    total <- nrow(plot_data)
    
    fields <- list(
      list(label = "Disease Reported", col = "disrep"),
      list(label = "Scale Reported", col = "scalerep"),
      list(label = "Satellite Reported", col = "satrep"),
      list(label = "Sensor Reported", col = "sensrep"),
      list(label = "Spatial Resolution Reported", col = "resrep"),
      list(label = "Geophysical Variables Reported", col = "georep"),
      list(label = "Terrestrial Variables Reported", col = "terrep")
    )
    
    rows <- lapply(fields, function(f) {
      count <- sum(tolower(trimws(plot_data[[f$col]])) == "yes", na.rm = TRUE)
      pct <- round(100 * count / total, 1)
      tags$tr(
        tags$td(style = "font-weight: bold; padding: 4px;", f$label),
        tags$td(style = "padding: 4px;", paste0(count, " / ", total, " (", pct, "%)"))
      )
    })
    
    div(
      style = "margin-top: 10px;",
      h4("Overall Reporting Summary", style = "color: #2c3e50; font-weight: bold;"),
      tags$table(style = "width: 100%; font-size: 14px;", do.call(tagList, rows))
    )
  })
  
    # output$chart_year <- renderPlot({
    #   # chart1 <- plot_data %>%
    #   #   filter(!is.na(event), !is.na(vector_family),
    #   #          nchar(trimws(event)) > 0) %>%
    #   #   count(event, vector_family) %>%
    #   #   complete(event, vector_family, fill = list(n = 0))
    # 
    #   ggplot(plot_data, aes(x = date)) +
    #     geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    #     scale_x_continuous(breaks = function(x) seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)) +
    #     # geom_text(aes(label = ifelse(n == 0, "", n)), color = "gray", size = 10) +
    #     # scale_fill_gradientn(
    #     #   colors = c("gray80", plasma(100)),
    #     #   values = scales::rescale(c(0, 0.001, 1)),
    #     #   name = "Count") +
    #     labs(
    #       title = "Number of Publications Found Per Year",
    #       x = "Year",
    #       y = "Number of Publications"
    #     ) +
    #     theme_minimal() +
    #     theme(
    #       axis.title.x = element_text(size = 15),
    #       axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    #       axis.title.y = element_text(size = 15),
    #       axis.text.y = element_text(size = 15),
    #       plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    #       axis.ticks = element_line(color = "black"),
    #       panel.grid.major = element_line(color = "gray80"),
    #       panel.grid.minor = element_blank()
    #       # panel.grid = element_blank()
    #     )
    # })
  
  output$chart_year <- renderPlot({
    var <- input$treemap_var
    # print(paste("var:", var))
    # 
    # ####DEBUGGING
    # raw <- plot_data %>% 
    #   mutate(selected_var = as.character(.data[[var]])) %>% 
    #   mutate(selected_var = ifelse(
    #     is.na(selected_var) | trimws(selected_var) == "" | selected_var == "NA",
    #     "Unknown", selected_var
    #   ))
    # print(paste("step 1 rows:", nrow(raw)))
    # print(head(raw$selected_var))
    # 
    # raw2 <- raw %>% 
    #   mutate(split_var = strsplit(selected_var, ",\\s*"))
    # print(paste("step 2 rows:", nrow(raw2)))
    # 
    # raw3 <- raw2 %>% 
    #   tidyr::unnest(cols = c(split_var))
    # print(paste("step 3 rows:", nrow(raw3)))
    # print(names(raw3))
    # 
    # chart_data <- raw3 %>% 
    #   mutate(selected_var = trimws(split_var)) %>% 
    #   filter(!is.na(date)) %>% 
    #   dplyr::count(date, selected_var)
    # print(paste("step 4 rows:", nrow(chart_data)))
    # print(head(chart_data))
    # 
    # if (nrow(chart_data) == 0) {
    #   return(ggplot() + labs(title = "No data") + theme_void())
    # }
    
    ### ORIGINAL WORKING CODE
    if(var == "date") {
      ggplot(plot_data, aes(x = date)) +
        geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
        scale_x_continuous(breaks = function(x) seq(floor(min(x, na.rm = TRUE)),
                                                    ceiling(max(x, na.rm = TRUE)), by = 1)) +
        labs(title = "Number of Publications Found Per Year",
             x = "Year", y = "Number of Publications") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
        )
    } else {

    label <- switch(var,
                    "NewSat" = "Satellite",
                    "disease" = "Disease",
                    "scale2" = "Study Scale",
                    "tervar" = "Terrestrial Variables",
                    "Sensor" = "Sensor",
                    "distype" = "Disease Type",
                    "genvar" = "Geophysical Variables",
                    "date" = "Year",
                    "rescat" = "Spatial Resolution")
    
    chart_data <- plot_data %>%
      dplyr::rename(selected_var = all_of(var)) %>%
      mutate(selected_var = ifelse(
        is.na(selected_var) | trimws (as.character(selected_var)) == "",
        "Unknown", as.character(selected_var)
      )) %>%
      filter(!is.na(date)) %>%
      dplyr::count(date, selected_var)
    
    # ERROR
    # chart_data <- tryCatch({plot_data %>% 
    #   expand_var(var) %>% 
    #   filter(!is.na(date)) %>% 
    #   dplyr::count(data, selected_var)
    # }, error = function(e) {
    #   print(paste("chart_data error:", e$message))
    #   return(NULL)
    # })
    # 
    # print(paste("chart_data rows:", nrow(chart_data)))
    # print(head(chart_data))
    
    # ERROR MESSAGE
    # if (is.null(chart_data) || nrow(chart_data) == 0) {
    #   return(ggplot() +
    #            labs(title = "No data available") +
    #            theme_void())
    # }
    
    
    
    unique_vals <- unique(chart_data$selected_var)
    print(paste("unique vals:", length(unique_vals)))
    
    ggplot(chart_data, aes(x = date, y = n, fill = selected_var)) +
      geom_bar(stat = "identity", color = "white", linewidth = 0.2) +
      scale_fill_manual(values = plasma(length(unique_vals)), name = label) +
      scale_x_continuous(
        breaks = function(x) seq(floor(min(x, na.rm = TRUE)), 
                                 ceiling(max(x, na.rm = TRUE)), by = 1)
        ) +
      # geom_text(aes(label = ifelse(n == 0, "", n)), color = "gray", size = 10) +
      # scale_fill_gradientn(
      #   colors = c("gray80", plasma(100)),
      #   values = scales::rescale(c(0, 0.001, 1)),
      #   name = "Count") +
      labs(
        title = paste("Publications Per Year by", label),
        x = "Year",
        y = "Number of Publications"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10)
        # panel.grid = element_blank()
      )
    }
  })
  
    # output$pie_sat <- renderPlot({
    #   
      # sat_counts <- plot_data %>%
      #   dplyr::count(NewSat)
    #   
    #   ggplot(sat_counts, aes(x = "", y = n, fill = NewSat)) +
    #     geom_bar(stat = "identity", width = 1, color = "white") +
    #     coord_polar("y", start = 0) +
    #     theme_void()
    # })
    # 
    # output$pie_sens <- renderPlot({
    #   
      # sens_counts <- plot_data %>%
      #   dplyr::count(Sensor)
    #   
    #   ggplot(sens_counts, aes(x = "", y = n, fill = Sensor)) +
    #     geom_bar(stat = "identity", width = 1, color = "white") +
    #     coord_polar("y", start = 0) +
    #     theme_void()
    # })
    
    # output$tree_sat <- renderPlot({
    # 
    #   sat_counts <- plot_data %>%
    #     dplyr::count(NewSat) %>%
    #     arrange(desc(n))
    # 
    #   treemap(sat_counts,
    #           index = "NewSat",
    #           vSize = "n",
    #           type = "categorical",
    #           vColor = "NewSat",
    #           palette = plasma(nrow(sat_counts)),
    #           title = "Satellites Used",
    #           legend = FALSE)
    # })
    
    output$tree_var <- renderPlot({
      var <- input$treemap_var
      label <- switch(var,
                      "NewSat" = "Satellite",
                      "disease" = "Disease",
                      "scale2" = "Study Scale",
                      "tervar" = "Terrestrial Variables",
                      "Sensor" = "Sensor",
                      "distype" = "Disease Type",
                      "genvar" = "Geophysical Variables",
                      "date" = "Year",
                      "rescat" = "Spatial Resolution")
      
      # counts <- plot_data %>%
      #   dplyr::rename(selected_var = all_of(var)) %>%
      #   mutate(selected_var = ifelse(
      #     is.na(selected_var) | trimws(as.character(selected_var)) == "",
      #     "Unknown", as.character(selected_var)
      #   )) %>% 
      #   dplyr::count(selected_var) %>% 
      #   arrange(desc(n))
      
      counts <- plot_data %>% 
        expand_var(var) %>% 
        dplyr::count(selected_var) %>% 
        arrange(desc(n))
      
      treemap(counts,
              index = "selected_var",
              vSize = "n",
              type = "index",
              vColor = "selected_var",
              palette = plasma(nrow(counts)),
              title = paste("Treemap -", label),
              legend = FALSE)
    })
    
    output$pie_var <- renderPlot({
      var <- input$treemap_var
      label <- switch(var,
                      "NewSat" = "Satellite",
                      "disease" = "Disease",
                      "scale2" = "Study Scale",
                      "tervar" = "Terrestrial Variables",
                      "Sensor" = "Sensor",
                      "distype" = "Disease Type",
                      "genvar" = "Geophysical Variables",
                      "date" = "Year",
                      "rescat" = "Spatial Resolution")
      # counts <- plot_data %>% 
      #   dplyr::rename(selected_var = all_of(var)) %>% 
      #   mutate(selected_var = ifelse(
      #     is.na(selected_var) | trimws(as.character(selected_var)) == "",
      #     "Unknown", as.character(selected_var)
      #   )) %>%
      #   dplyr::count(selected_var) %>% 
      #   arrange(desc(n))
      
      counts <- plot_data %>% 
        expand_var(var) %>% 
        dplyr::count(selected_var) %>% 
        arrange(desc(n))
      
      ggplot(counts, aes(x = "", y = n, fill = selected_var)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = plasma(nrow(counts)), name = label) +
        labs(title = paste("Pie Chart -", label)) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right",
          legend.text = element_text(size = 9),
          legend.title = element_text(face = "bold", size = 10)
        )
    })
    
    # output$tree_sens <- renderPlot({
    #   
    #   sens_counts <- plot_data %>%
    #     dplyr::count(Sensor) %>% 
    #     arrange(desc(n))
    #   
    #   treemap(sens_counts,
    #           index = "Sensor",
    #           vSize = "n",
    #           type = "categorical",
    #           vColor = "Sensor",
    #           palette = plasma(nrow(sens_counts)),
    #           title = "Sensors Used",
    #           legend = FALSE)
    # })
    
    # output$binary_heatmap <- renderPlot({
    #   heatmap_data <- plot_data %>% 
    #     select(title, disrep, scalerep, satrep, sensrep, resrep, georep, terrep) %>% 
    #     mutate(across(-title, ~tolower(trimws(as.character(.))))) %>% 
    #     mutate(across(-title, ~ifelse(. == "yes", 1, 0))) %>% 
    #     pivot_longer(
    #       cols = -title,
    #       names_to = "variable",
    #       values_to = "reported"
    #     ) %>% 
    #     mutate(
    #       variable = recode(variable,
    #                         "disrep" = "Disease",
    #                         "scalerep" = "Scale",
    #                         "satrep" = "Satellite Used",
    #                         "sensrep" = "Sensor Used",
    #                         "resrep" = "Spatial Resolution",
    #                         "georep" = "Geophysical Variables",
    #                         "terrep" = "Terrestrial Variables"),
    #       reported = factor(reported, levels = c(0,1),
    #                         labels = c("Not Reported", "Reported"))
    #     )
    #   
    #   ggplot(heatmap_data, aes(x = variable, y = reorder(title, title), fill = reported)) +
    #     geom_tile(color = "white", linewidth = 0.3) +
    #     scale_fill_manual(
    #       values = c("Not Reported" = "#d4e6f1", "Reported" = "#1a5276"),
    #       name = ""
    #     ) +
    #     labs(
    #       title = "Reporting Criteria by Article",
    #       x = "Variable",
    #       y = "Article"
    #     ) +
    #     theme_minimal() +
    #     theme(
    #       axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
    #       axis.text.y = element_text(size = 7),
    #       plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    #       legend.position = "top"
    #     )
    # })
    
    output$binary_heatmap <- renderPlot({
      heatmap_data <- plot_data %>%
        select(title, disrep, scalerep, satrep, sensrep, resrep, georep, terrep) %>%
        mutate(across(-title, ~tolower(trimws(as.character(.))))) %>%
        mutate(across(-title, ~ifelse(. == "yes", 1, 0))) %>%
        pivot_longer(
          cols = -title,
          names_to = "variable",
          values_to = "reported"
        ) %>%
        mutate(
          variable = recode(variable,
                            "disrep" = "Disease",
                            "scalerep" = "Scale",
                            "satrep" = "Satellite Used",
                            "sensrep" = "Sensor Used",
                            "resrep" = "Spatial Resolution",
                            "georep" = "Geophysical Variables",
                            "terrep" = "Terrestrial Variables"),
          reported = factor(reported, levels = c(0,1),
                            labels = c("Not Reported", "Reported")),
          short_title = ifelse(
            nchar(title) > 50,
            paste0(substr(title, 1, 50), "..."),
            title
          )
        )

      ggplot(heatmap_data, aes(x = variable, y = reorder(short_title, short_title), fill = reported)) +
        geom_tile(color = "white", linewidth = 0.3) +
        scale_fill_manual(
          values = c("Not Reported" = "#d4e6f1", "Reported" = "#1a5276"),
          name = ""
        ) +
        labs(
          title = "Reporting Criteria by Article",
          x = "Variable",
          y = NULL
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "top"
        )
    })
    
    # output$binary_heatmap <- renderPlotly({
    #   heatmap_data <- plot_data %>% 
    #     select(title, disrep, scalerep, satrep, sensrep, resrep, georep, terrep) %>% 
    #     mutate(across(-title, ~tolower(trimws(as.character(.))))) %>% 
    #     mutate(across(-title, ~ifelse(. == "yes", 1, 0))) %>% 
    #     pivot_longer(
    #       cols = -title,
    #       names_to = "variable",
    #       values_to = "reported"
    #     ) %>% 
    #     mutate(
    #       variable = recode(variable,
    #                         "disrep" = "Disease",
    #                         "scalerep" = "Scale",
    #                         "satrep" = "Satellite Used",
    #                         "sensrep" = "Sensor Used",
    #                         "resrep" = "Spatial Resolution",
    #                         "georep" = "Geophysical Variables",
    #                         "terrep" = "Terrestrial Variables"),
    #       reported = factor(reported, levels = c(0,1),
    #                         labels = c("Not Reported", "Reported")),
    #       short_title = ifelse(
    #         nchar(title) > 40,
    #         paste0(substr(title, 1, 40), "..."),
    #         title
    #       )
    #     )
    #   
    #   p <- ggplot(heatmap_data, aes(x = variable, 
    #                                 y = reorder(short_title, short_title), 
    #                                 fill = reported,
    #                                 text = paste0("<b>Article:</b> ", title,
    #                                               "<br><b>Variable:</b> ", variable,
    #                                               "<br><b>Status:</b> ", reported))) +
    #     geom_tile(color = "white", linewidth = 0.3) +
    #     scale_fill_manual(
    #       values = c("Not Reported" = "#d4e6f1", "Reported" = "#1a5276"),
    #       name = ""
    #     ) +
    #     labs(
    #       title = "Reporting Criteria by Article",
    #       x = "Variable",
    #       y = NULL
    #     ) +
    #     theme_minimal() +
    #     theme(
    #       axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
    #       axis.text.y = element_text(size = 9),
    #       plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    #       legend.position = "top"
    #     )
    #   
    #   ggplotly(p, tooltip = "text") %>% 
    #     layout(
    #       yaxis = list(fixedrange = FALSE),
    #       xaxis = list(fixedrange = TRUE)
    #     ) %>% 
    #     config(
    #       scrollZoom = TRUE,
    #       displayModeBar = TRUE,
    #       modeBarButtonsToRemove = list("select2d", "lasso2d")
    #     )
    # })
    
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      # print("click detected")
      # print(paste("lat:", click$lat, "lng:", click$lng))


      # Identify the row by matching the lat/long of the clicked marker
      row <- plot_data %>%
        filter(abs(lat_j - click$lat) < 0.01, abs(long_j - click$lng) < 0.01) %>%
        slice(1)
      print(paste("rows found:", nrow(row)))
      print(row)


        output$click_info <- renderUI({
          print("inside renderUI")

          tryCatch({

          #   safe <- function(x) {
          #     x <- as.character(x)
          #     # x <- ifelse(is.na(x) | trimws(x) == "" | x == "NA", "Not reported", x)
          #     x <- gsub('"', '', x)   # remove embedded quotes
          #     x <- gsub("'", "", x)   # remove embedded single quotes
          #     x
          #   }
          # 
          # source_display <- if (is.na(row$link) || trimws(row$link) == "" ||
          #                       !grepl("^https?://", trimws(row$link))) {
          #   tags$td(style = "padding: 5px;", "No source available")
          # } else {
          #   tags$td(style = "padding: 5px;",
          #           tags$a(href = trimws(row$link),
          #                  target = "_blank",
          #                  "Website Link"))
          # }

          div(
            style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd;",
            h3(paste0(row$title)),
            tags$table(
              style = "width: 100%; font-size: 16px;",
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Location:"),
                tags$td(style = "padding: 5px;", row$location)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Year:"),
                tags$td(style = "padding: 5px;", row$date)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Disease:"),
                tags$td(style = "padding: 5px;", row$disease)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Disease Type:"),
                tags$td(style = "padding: 5px;", row$distype)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Satellites/Sensors Used:"),
                tags$td(style = "padding: 5px;", row$sat)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Spatial Resolution:"),
                tags$td(style = "padding: 5px;", row$res)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Geophysical Environmental Variables:"),
                tags$td(style = "padding: 5px;", row$genvar)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Terrestrial Variables Included?"),
                tags$td(style = "padding: 5px;", row$tervar)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Study Scale:"),
                tags$td(style = "padding: 5px;", row$scale)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px;", "Study Objective:"),
                tags$td(style = "padding: 5px;", row$obj)
              ),
              tags$tr(
                tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Source:"),
                tags$td(style = "padding: 5px;",
                        tags$a(href = row$DOI,
                               target = "_blank",
                               row$DOI)
                )
              )
            )
          )

        }, error = function(e) {
          div(
            style = "padding: 15px; background-color: #fff3cd; border-radius: 8px; border: 1px solid #ffc107;",
            h4("Could not load details for this marker."),
            p(paste("Error:", e$message))
          )
        })
    })
  })
}

shinyApp(ui, server)

# locations <- locations %>% 
#   mutate(
#     disease = replace_na(disease, "Nuisance"),
#     disease = if_else(trimws(disease) == "None", "Nuisance", disease))

# ui <- navbarPage("PAND_EOS Articles Catalog",
#     
#     # header = tags$head(
#     #   tags$style(HTML("
#     #       #Make the sidebars look cleaner
#     #       .sidebar-header {
#     #         color: #2c3e50;
#     #         font-weight: bold;
#     #         margin-top: 20px;
#     #       }
#     #       
#     #       # Style the main instructional text
#     #       .instruction_text {
#     #       font-size: 16px;
#     #       line-height: 1.6;
#     #       color: #444444;
#     #       background-color: #f9f9f9;
#     #       padding: 15px;
#     #       border-left: 5px solid #007bc2;
#     #       border-radius: 4px;
#     #       }
#     #       
#     #       #Title text
#     #       .title-header {
#     #       text-align: center;
#     #       padding: 20px;
#     #       color: #2c3e50;
#     #       width: 100%;
#     #       }
#     #       "))
#     #              ),
#     # 
#     # tabPanel("Home",
#     #          titlePanel(
#     #            div(class = "title-header", "Welcome to Your Global Catalog of Natural Disasters and Disease Vectors")),
#     #          
#     #          sidebarLayout(
#     #            sidebarPanel(
#     #              div(
#     #                style = "text-align: center;",
#     #                img(src = "www/logo_pic.png", height = 125, width = 275),
#     #              ),
#     #              h3("About Us", class = "sidebar-header"),
#     #              p("The Quantitative Disease Ecology and Conservation Lab group led by Dr. Sadie Ryan
#     #                at the University of Florida is an interdisciplinary team of geographers, public health scientists, 
#     #                and disease ecologists using innovative geospatial modeling techniques to providing communities 
#     #                with the information they need to combat diseases transmitted by mosquitoes and ticks. One of the main
#     #                objectives of this research team is to analyze the effects of climate conditions on the survival likelihood of 
#     #                insects capable of transmitting disease, known as 'disease vectors' in different habitats.
#     #                This dashboard is designed to communicate these scientific concepts in a way that is understandable and
#     #                interactive, which aligns closely with QDEC's goals to be kind and do good science. Thank you for taking
#     #                the time to use this dashboard."
#     #              ),
#     #              p("Learn more about the QDEC lab here:",
#     #                tags$a(href = "https://qdec.geog.ufl.edu/", "https://qdec.geog.ufl.edu/", target = "_blank")),
#     #              hr(),
#     #              h3("About the Dashboard", class = "sidebar-header"),
#     #              p("The idea for this dashboard came from the lack of any singular source on global events of natural disasters followed by increased burden of biting insects 
#     #                and the diseases that they cause. While it is common knowledge that these two processes are closely related, the lack of available data showing this connection 
#     #                prompted us to conduct a review of all mentions of vectorborne disease and insect population booms following natural disasters. The goal is to show that there 
#     #                have been repeated occurrences, with increasing frequency in recent decades due to climate change, of this process along with highlighting the systemic issues 
#     #                contributing to increased disease and general vector burden."
#     #              ),
#     #              hr(),
#     #              h3("How Do Natural Disasters Affect Vector Populations?", class = "sidebar-header"),
#     #              p("What is a vector?"),
#     #              tags$ul(
#     #                tags$li("A disease vector is a living organism, usually one which feeds on animal and human blood, that transmits infections between humans and animals"),
#     #                tags$li("Examples include:",
#     #                        tags$ul(
#     #                          tags$li("Mosquitoes"),
#     #                          tags$li("Ticks"),
#     #                          tags$li("Fleas"),
#     #                          tags$li("Biting flies (sandflies and deerflies)")
#     #                        )),
#     #                ),
#     #              div(
#     #                style = "display: flex; justify-content: center; gap: 20px",
#     #                img(src = "www/mosquito_pic.png", height = 150, width = 225),
#     #                img(src = "www/blackfly_pic.png", height = 150, width = 225),
#     #              ),
#     #              div(
#     #                style = "display: flex; justify-content: center; gap: 20px",
#     #                img(src = "www/sandfly_pic.png", height = 150, width = 225),
#     #                img(src = "www/tick_pic.png", height = 150, width = 225),
#     #              ),
#     #              p("What is a natural disaster?"),
#     #              tags$ul(
#     #                tags$li("Major negative event in a vulnerable community that is caused by the impacts of a natural hazard and which typically involves human injury, death, or damage to property"),
#     #                tags$li("Examples include:",
#     #                        tags$ul(
#     #                          tags$li("Tropical Cyclones",
#     #                                  tags$ul(
#     #                                    tags$li("Known by different names in different parts of the world (Hurricanes, Tropical Storms, Typhoons, Cyclones)"),
#     #                                    tags$li("Rotating storm system with strong winds and heavy rain")
#     #                                  )),
#     #                          tags$li("Tornadoes",
#     #                                  tags$ul(
#     #                                    tags$li("Rotating, short-lived storm system with extremely strong winds that extend from a thunder cloud to the ground as a funnel")
#     #                                  )),
#     #                          tags$li("Floods",
#     #                                  tags$ul(
#     #                                    tags$li("Rapid, unexpected, large amounts of water arriving at a place where they are not usually"),
#     #                                    tags$li("Can occur from extreme rainfall, snowmelt, rivers and lakes overflowing, storm surge from tropical storms"),
#     #                                    tags$li("Can cause landslides and mudslides which move a large amount of dirt and water")
#     #                                  )),
#     #                          tags$li("Earthquakes",
#     #                                  tags$ul(
#     #                                    tags$li("Shaking of the Earth’s surface due to collisions between the tectonic plates underlying the surface of the world")
#     #                                  )),
#     #                          tags$li("Wildfires",
#     #                                  tags$ul(
#     #                                    tags$li("Large, uncontrolled fires that burn in dry areas with plenty of vegetation and can spread to human settlements if not contained")
#     #                                  )),
#     #                        )
#     #                )
#     #                ),
#     #              div(
#     #                style = "display: flex; justify-content: center; gap: 20px",
#     #                img(src = "www/flood_pic.png", height = 150, width = 225),
#     #                img(src = "www/hurricane_pic.png", height = 150, width = 225),
#     #              ),
#     #              div(
#     #                style = "display: flex; justify-content: center; gap: 20px",
#     #                img(src = "www/quake_pic.png", height = 150, width = 225),
#     #                img(src = "www/damage_pic.png", height = 150, width = 225),
#     #              ),
#     #              p("What is climate change?"),
#     #              tags$ul(
#     #                tags$li("The ongoing process of changing of environmental conditions including temperature and precipitation patterns in different areas around the world"),
#     #                tags$li("Human activity, mainly contribution of “greenhouse gas” emissions like carbon dioxide, have accelerated the amount of climate change occurring"),
#     #                tags$li("Impacts different places in different ways, but generally makes places hotter and either extremely wet or extremely dry")
#     #              ),
#     #              p("How is climate change related to natural disasters and vectors?"),
#     #              tags$ul(
#     #                tags$li("Climate change is directly related to so many natural disasters because as temperatures rise, the frequency and severity of many disasters like floods, 
#     #                        tropical storms, droughts, wildfires, and even seasonal weather patterns increase significantly"),
#     #                tags$li("With more frequent and severe natural disasters, there is a growing potential for insects that transmit disease to reproduce and spread to new areas that they have not previously lived in"),
#     #                tags$li("This exposes an increasingly larger amount of people to the diseases that these insects carry")
#     #              ),
#     #              div(
#     #                style = "text-align: center;",
#     #                img(src = "www/systems_pic.png", height = 400, width = 550),
#     #              )
#     #          ),
#     #          
#     #          mainPanel(
#     #            h1("User Guide"),
#     #            h3("Note From the Creators", class = "sidebar-header"),
#     #            p("While the data collection process for this dashboard attempted to be as comprehensive as possible (more details provided below) there are undoubtedly instances of natural disasters 
#     #            impacting vector populations that were not accounted for in this dashboard due to the sheer volume and frequency of occurrences. However, we went to great lengths to search for occurrences 
#     #            in every country in order to provide an overview of global patterns at play. It is our goal to update this dashboard as future events occur to continue strengthening the results and 
#     #            providing people with the information that they need to understand this issue."),
#     #            h3("About the Data", class = "sidebar-header"),
#     #            p("How was it collected?"),
#     #            tags$ul(
#     #              tags$li("Countries for analysis were gathered from the official World Health Organization designated list"),
#     #              tags$li("For every country, an internet search was conducted using keywords such as “floods” and “mosquitoes”, and depending on the region “hurricanes”, “typhoons”, “monsoons” 
#     #                              or just generally “natural disasters” with the word “mosquitoes” or “insects” to find mentions of insect populations growing and diseases spreading after natural disasters")
#     #            ),
#     #            p("Gaps in the Data"),
#     #            tags$ul(
#     #              tags$li("The two primary gaps in the data pertained to the diseases reported or of concern as well as the vector species reported, as many places reported upticks in insect populations 
#     #                              but did not say that they caused a specific disease or mention the actual name of the species"),
#     #              tags$li("For events with no disease being mentioned in the source, they were categorized as “Nuisance” in the display because while they may not have been documented as causing a disease 
#     #                              outbreak they were consistently reported as biting people, therefore making them a nuisance"),
#     #              tags$li("For events with no specific insect vector being mentioned in the source, they were categorized as “Unknown mosquitoes” since the articles would frequently simply say “mosquitoes” 
#     #                              or “disease-transmitting mosquitoes” which clearly shows the connection between disasters and vectors but just doesn’t say a certain species")
#     #            ),
#     #            h3("Options Overview", class = "sidebar-header"),
#     #            p("This dashboard has dropdown menus that allow you to select and zoom in on different areas around the world for easier navigation depending on what your area of interest is. You can also 
#     #            freely zoom wherever you want in the map, but this tool is to aid with slightly faster navigation. This dashboard also has buttons for selecting what variables you want to view in the map display."),
#     #            h3("Customizable Options", class = "sidebar-header"),
#     #            p("Geographic Area"),
#     #            tags$ul(
#     #              tags$li("Region:",
#     #                      tags$ul(
#     #                        tags$li("This level is by continent, with all of the options (North America, South America, Africa, Asia, Europe, and Oceania) zooming in on different continents around the world for a slightly more concentrated view of the events."),
#     #                        tags$li("Disclaimer: for continents with countries that have overseas dependencies, such as many European countries, there will not be a significant difference from the world view and the continent view.")
#     #                      )),
#     #              tags$li("Country:",
#     #                      tags$ul(
#     #                        tags$li("This level is by country, with all of the options zooming in on different countries around the world for a more specific view of the events.")
#     #                      )),
#     #            ),
#     #            p("Total Observations"),
#     #            tags$ul(
#     #              tags$li("Selecting this button will populate a map view that has each of the world’s countries color coded based on the number of observations found online that associate natural disasters with changes in insect population and disease outbreak"),
#     #              tags$li("You can then mouse over the map and as you go over each country, a label will appear that will tell you the number of observations seen in that country."),
#     #              div(
#     #                style = "text-align: center;",
#     #                img(src = "www/areaselect_pic.png", height = 375, width = 750),
#     #              ),
#     #            ),
#     #            hr(),
#     #            p("Event Type"),
#     #            tags$ul(
#     #              tags$li("Selecting this button will populate a map view that has points representing every location mentioned in any of the articles color coded by the type of natural event that occurred there"),
#     #              tags$li("By hovering over any of the points, all of the general information about the event will populate in a label. By clicking on any of the points, the information will populate below the map along with a link to the original source for more information if you are curious"),
#     #              tags$li("Note: “Climate” and “Seasonal” are mentioned alongside more acute natural disasters. This was done intentionally to demonstrate the long-term impacts that climate change is having in specific areas that are cited as having insect populations growing and more disease outbreaks because of climate change and severe seasonal weather changes")
#     #            ),
#     #            div(
#     #              style = "text-align: center;",
#     #              img(src = "www/eventselect_pic.png", height = 375, width = 750),
#     #            ),
#     #            p("Disease"),
#     #            tags$ul(
#     #              tags$li("Selecting this button will populate a map view that has points representing every location mentioned in any of the articles color coded by the type of disease (if any) that was reported to increase after the event"),
#     #              tags$li("By hovering over any of the points, all of the general information about the event will populate in a label. By clicking on any of the points, the information will populate below the map along with a link to the original source for more information if you are curious"),
#     #              tags$li("Note: each point on the map is color coded only by one disease, but many of them have multiple diseases reported as increasing following natural disasters. For each article, the primary disease reported or main disease of concern was reported as the disease used for the point color to reduce the number of diseases displayed in the legend. However, all diseases of concern and those reported are included in the hover label and description")
#     #            ),
#     #            div(
#     #              style = "text-align: center;",
#     #              img(src = "www/diseaseselect_pic.png", height = 375, width = 750),
#     #            ),
#     #            p("Vector Species"),
#     #            tags$ul(
#     #              tags$li("Selecting this button will populate a map view that has points representing every location mentioned in any of the articles color coded by the species of insect (if any) that was reported to increase after the event"),
#     #              tags$li("By hovering over any of the points, all of the general information about the event will populate in a label. By clicking on any of the points, the information will populate below the map along with a link to the original source for more information if you are curious"),
#     #              tags$li("Note: as with the disease display, each point on the map is color coded only by one family of insect vector to consolidate the information for the sake of displaying in the map. This was decided either by finding the vector that was mentioned most throughout an article or by selecting a vector that had not been seen frequently throughout the rest of the sources (such as Blackflies and Sandflies) to provide more diversity in the data rather than reporting solely mosquitoes for all points")
#     #            ),
#     #            div(
#     #              style = "text-align: center;",
#     #              img(src = "www/vectorselect_pic.png", height = 375, width = 750),
#     #            ),
#     #          )
#     #          )
#     # ),
#                               
#     tabPanel(
#       "Articles Map",
#       titlePanel("Global Articles Map"),
#       
#       # sidebarLayout(
#       #   sidebarPanel(
#       #     p("Select a location to zoom the camera to that region"),
#       #     
#       #     selectInput("region_select", "Zoom to region", choices = NULL),
#       #     selectInput("country_select", "Zoom to country", choices = NULL),
#       #     
#       #     hr(),
#       #     
#       #     radioButtons(
#       #       inputId = "color_by",
#       #       label = "Color Markers By:",
#       #       choices = c(
#       #         "Total Observations" = "none",
#       #         "Event Type" = "event",
#       #         "Disease" = "disease",
#       #         "Vector Species" = "species"
#       #       ),
#       #       selected = "none"
#       #     ),
#       #     
#       #     p("Use the map to click on the markers for more details"),
#       #     
#       #     hr(),
#       #     
#       #     uiOutput("click_info")
#           # 
#           # h3("Heat Map (Below)", class = "sidebar-header"),
#           # p("What do these show?"),
#           # tags$ul(
#           #   tags$li("Heat maps are used to show the number of times that two factors overlap, 
#           #           and are used to understand patterns when there are a lot of options to consider within these two factors"),
#           #   tags$li("Colors are representative of the number of times that the variable overlap, and is shown in the legend to the right of the graph")
#           # ),
#           # p("Natural Events vs. Vector Family"),
#           # tags$ul(
#           #   tags$li("Shows the number of times that certain families of insect vectors (such as Aedes, Anopheles, Culex, etc.) were seen following different disasters (such as typhoons, floods, earthquakes, etc.)"),
#           #   tags$li("Lowest values are in dark red, highest values are in bright yellow"),
#           #   tags$li("Number of occurrences is written inside of each square")
#           # ),
#           # p("Natural Events vs. Disease"),
#           # tags$ul(
#           #   tags$li("Shows the number of times that certain diseases (such as Dengue Fever, Malaria, Zika, etc.) were seen following different disasters"),
#           #   tags$li("Lowest values are in dark red, highest values are in bright yellow"),
#           #   tags$li("Number of occurrences is written inside of each square")
#           # ),
#           # p("General Themes from the Data"),
#           # tags$ul(
#           #   tags$li("Flooding, climate change, and hurricanes are the three most frequently cited natural hazards related to increased burden of disease vectors"),
#           #   tags$li("Aedes, Anopheles, and Culex mosquitoes are most frequently cited, along with unknown or unmentioned mosquito species"),
#           #   tags$li("Flooding, climate change, and hurricanes also caused the most diverse array of diseases, with flooding in particular leading to a large range of outcomes"),
#           #   tags$li("Dengue Fever, Malaria, and Chikungunya were the most common diseases cited as occurring after natural disasters, with general nuisance biting being the most common issue overall")
#           # ),
#           # p("What does this mean?"),
#           # tags$ul(
#           #   tags$li("Natural disasters which introduce large amounts of water into new areas lead to mosquito growth"),
#           #   tags$li("Aedes family of mosquitoes, which happen to be the vectors of the most common diseases, tend to have population growth after natural disasters"),
#           #   tags$li("Broadly, a lack of specificity when reporting on these issues could lead to reduced ability to stop the vectors and their diseases, with Unknown and Nuisance being the two most common answers by far for Vector Family and Disease Transmitted"),
#           #   tags$li("Policymakers need to incorporate vector control, specifically for Aedes mosquitoes, as well as disease surveillance into their disaster preparedness plans to avoid outbreaks")
#           # )
#         # ),
#         
#         mainPanel(
#           leafletOutput("map", height = 700),
#           # hr(),
#           # uiOutput("click_info")
#         )
#       )
# )
#       
#       # hr(),
#       # fluidRow(
#       #   column(12,
#       #          h3("Heat Map Interpretation", class = "sidebar-header"),
#       #          p("What do these show?"),
#       #          tags$ul(
#       #            tags$li("Heat maps are used to show the number of times that two factors overlap, 
#       #               and are used to understand patterns when there are a lot of options to consider within these two factors"),
#       #            tags$li("Colors are representative of the number of times that the variable overlap, and is shown in the legend to the right of the graph")
#       #          ),
#       #          p("Natural Events vs. Vector Family"),
#       #          tags$ul(
#       #            tags$li("Shows the number of times that certain families of insect vectors (such as Aedes, Anopheles, Culex, etc.) were seen following different disasters (such as typhoons, floods, earthquakes, etc.)"),
#       #            tags$li("Lowest values are in dark red, highest values are in bright yellow"),
#       #            tags$li("Number of occurrences is written inside of each square")
#       #          ),
#       #          p("Natural Events vs. Disease"),
#       #          tags$ul(
#       #            tags$li("Shows the number of times that certain diseases (such as Dengue Fever, Malaria, Zika, etc.) were seen following different disasters"),
#       #            tags$li("Lowest values are in dark red, highest values are in bright yellow"),
#       #            tags$li("Number of occurrences is written inside of each square")
#       #          ),
#       #          p("General Themes from the Data"),
#       #          tags$ul(
#       #            tags$li("Flooding, climate change, and hurricanes are the three most frequently cited natural hazards related to increased burden of disease vectors"),
#       #            tags$li("Aedes, Anopheles, and Culex mosquitoes are most frequently cited, along with unknown or unmentioned mosquito species"),
#       #            tags$li("Flooding, climate change, and hurricanes also caused the most diverse array of diseases, with flooding in particular leading to a large range of outcomes"),
#       #            tags$li("Dengue Fever, Malaria, and Chikungunya were the most common diseases cited as occurring after natural disasters, with general nuisance biting being the most common issue overall")
#       #          ),
#       #          p("What does this mean?"),
#       #          tags$ul(
#       #            tags$li("Natural disasters which introduce large amounts of water into new areas lead to mosquito growth"),
#       #            tags$li("Aedes family of mosquitoes, which happen to be the vectors of the most common diseases, tend to have population growth after natural disasters"),
#       #            tags$li("Broadly, a lack of specificity when reporting on these issues could lead to reduced ability to stop the vectors and their diseases, with Unknown and Nuisance being the two most common answers by far for Vector Family and Disease Transmitted"),
#       #            tags$li("Policymakers need to incorporate vector control, specifically for Aedes mosquitoes, as well as disease surveillance into their disaster preparedness plans to avoid outbreaks")
#       #          )
#       #   )),
#       # 
#       #   hr(),
#       #   fluidRow(
#       #     column(6, plotOutput("heatmap_disease", height = 600)),
#       #     column(6, plotOutput("heatmap_species", height = 600))
#       #   ),
#       # 
#       # hr(),
#       # fluidRow(
#       #   column(5,
#       #          div(
#       #            style = "text-align: center;",
#       #            img(src = "www/tiptoss_pic.png", height = 500, width = 575),
#       #          ),
#       #   ),
#       #   column(7,
#       #          tags$div(
#       #            id = "comp_prep",
#       #            style = "flex: 1; padding: 12px; font-size: 14px; border: none; outline: none; overflow-y: auto; line-height: 1.6;",
#       #            tags$h3("How Do I Protect Myself?"),
#       #            p("Prevention is critical when trying to stop Dengue"),
#       #            p("If we can stop the mosquitoes that spread the disease, we can stop it from circulating"),
#       #            p("Personal Protection: Stopping Mosquitoes from Biting You"),
#       #            tags$ul(
#       #              tags$li("Wear clothes that cover as much of your body as possible"),
#       #              tags$li("Use mosquito nets if sleeping during the day"),
#       #              tags$li("Install window screens in your home"),
#       #              tags$li("Use approved mosquito repellents (containing DEET, Picaridin, or IR3535)",),
#       #              tags$li("Install vaporizers and traps for mosquitoes outside of your home"),
#       #              tags$li("Avoid going outside during peak biting hours (dawn and dusk)"),
#       #            ),
#       #            p("Environmental Protection: Tip and Toss"),
#       #            tags$ul(
#       #              tags$li("Trying to reduce the places where mosquitoes like to reproduce to reduce their populations"),
#       #              tags$li("Tip over any containers with excess standing water"),
#       #              tags$li("Toss any containers or other items that collect rain"),
#       #              tags$li("Apply approved insecticides to outdoor water containers that cannot be tossed"),
#       #              tags$li("Ask your local health department or vector control unit about “Mosquitofish” which can be added to water containers and feed on mosquito larvae")
#       #            ),
#       #            p("Many of these efforts can be inhibited by natural disaster impacts"),
#       #            tags$ul(
#       #              tags$li("Encourage your local health department and emergency management agency to have plans to deal with insect vectors that are native to your area before disasters strike"),
#       #              tags$li("Rapid cleanup following natural disasters is critical to reducing artificial breeding sites for mosquitos and other vectors"),
#       #            ),
#       #            p("Ask your local health department or vector control unit to trap and test mosquitoes found on your property to better support their prevention efforts"),
#       #          ) 
#       #   )
#       # )
# #     )
# # )
# 
# server <- function(input, output, session) {
#   
#   # observe({
#   #   reg_choices <- sort(unique(locations$continent[!is.na(locations$continent)]))
#   #   updateSelectInput(session, "region_select",
#   #                     choices = c("Jump to region..." = "", reg_choices))
#   # })
#   # 
#   # observeEvent(input$region_select, {
#   #   if(input$region_select == "") {
#   #     df_c <- locations
#   #   } else {
#   #     df_c <- locations %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   c_choices <- sort(unique(df_c$country[!is.na(df_c$country)]))
#   #   updateSelectInput(session, "country_select",
#   #                     choices = c("Jump to country..." = "", c_choices))
#   # })
#   
#   # plot_data <- locations %>%
#   #   filter(!is.na(lat_j), !is.na(long_j)) %>%
#   #   mutate(
#   #     disease = replace_na(disease, "Nuisance"),
#   #     disease = if_else(trimws(disease) == "None", "Nuisance", disease),
#   #     primary_disease = trimws(sapply(strsplit(as.character(disease), ","), '[', 1)),
#   #     vector_family = case_when(
#   #     grepl("Aedes", species, ignore.case = TRUE) ~ "Aedes",
#   #     grepl("Anopheles", species, ignore.case = TRUE) ~ "Anopheles",
#   #     grepl("Culex", species, ignore.case = TRUE) ~ "Culex",
#   #     grepl("Culicoides", species, ignore.case = TRUE) ~ "Culicoides",
#   #     grepl("Ochlerotatus", species, ignore.case = TRUE) ~ "Ochlerotatus",
#   #     grepl("Blackflies", species, ignore.case = TRUE) ~ "Blackflies",
#   #     grepl("Sandflies", species, ignore.case = TRUE) ~ "Sandflies",
#   #     grepl("Unknown", species, ignore.case = TRUE) ~ "Unknown"
#   #   ))
# 
#   # marker_colors <- reactive({
#   #   color_by <- input$color_by
#   #   
#   #   n <- nrow(plot_data)
#   #   
#   #   if (n == 0) return(character(0))
#   #   
#   #   if (is.null(color_by) || color_by == "none") {
#   #     return(rep("blue", n))
#   #   }
#     
#     # col <- case_when(
#     #   color_by == "species" ~ "vector_family",
#     #   color_by == "disease" ~ "primary_disease",
#     #   TRUE ~ color_by
#     # )
#     # 
#     # values <- plot_data[[col]]
#     # 
#     # values[is.na(values) | trimws(values) == ""] <- "Unknown"
#     # 
#     # family_colors <- c(
#     #   "Aedes" = "#0D0887FF",
#     #   "Anopheles" = "#4C02A1FF",
#     #   "Culex" = "#A92395FF",
#     #   "Culicoides" = "#CC4678FF",
#     #   "Ochlerotatus" = "#E56B5DFF",
#     #   "Blackflies" = "#7E03A8FF",
#     #   "Sandflies" = "#FDC328FF",
#     #   "Unknown" = "#999999"
#     # )
#     # 
#     # if (color_by == "species") {
#     #   palette <- colorFactor(
#     #     palette = family_colors,
#     #     domain = names(family_colors)
#     #   ) 
#     # } else {
#     #   unique_vals <- unique(values)
#     #   palette <- colorFactor(
#     #                            palette = plasma(length(unique_vals)),
#     #                            domain = unique_vals)
#     # }
#     # 
#     # # palette <- colorFactor(
#     # #   palette = rainbow(length(unique(values))),
#     # #   domain = unique(values)
#     # # )
#     # 
#     # palette(values)
#   # })
#   
#   # nrow(plot_data)
#   # head(plot_data)
#   
#   # output$map <- renderLeaflet({
#   #   
#   #   leaflet() %>% 
#   #     addTiles() %>% 
#   #     setView(lng = 0, lat = 30, zoom = 1.75) %>% 
#   #     addCircleMarkers(data = plot_data,
#   #                     lng = ~long_j,
#   #                     lat = ~lat_j,
#   #                     radius = 4,
#   #                     color = "blue",
#   #                     fillOpacity = 0.8,
#   #                     label = lapply(paste0(
#   #                     "<b>Location:</b> ", plot_data$location, ", ", plot_data$country, "<br>",
#   #                     "<b>Year:</b> ", plot_data$year, "<br>",
#   #                     "<b>Event:</b> ", plot_data$event, "<br>",
#   #                     "<b>Reported Disease:</b> ", plot_data$disease, "<br>",
#   #                     "<b>Disease of Concern:</b> ", plot_data$concern, "<br>",
#   #                     "<b>Species:</b> ", plot_data$species),
#   #                     HTML),
#   #                     layerId = ~paste0(lat_j, "_", long_j))
#   # })
#   
#   output$map <- renderLeaflet({
#     
#     # country_counts <- plot_data %>% 
#     # count(country) %>% 
#     #   rename(n_events = n)
#     # 
#     # world_counts <- world %>% 
#     #   left_join(country_counts, by = c("geounit" = "country")) %>% 
#     #   mutate(n_events = replace_na(n_events, 0))
#     # 
#     # pal <- colorNumeric(
#     #   palette = plasma(100),
#     #   domain = world_counts$n_events,
#     #   na.color = "#999999"
#     # )
#     
#     leaflet() %>% 
#       addTiles() %>% 
#       setView(lng = 0, lat = 30, zoom = 1.75) %>% 
#       # addPolygons(
#       #   data = world_counts,
#       #   fillColor = ~pal(n_events),
#       #   fillOpacity = 0.7,
#       #   color = "white",
#       #   weight = 1 %>% 
#       #   label = lapply(paste0(
#       #     "<b>", world_counts$geounit, "</b><br>",
#       #     "Observations: ", world_counts$n_events),
#       #     HTML)
#       #   ) %>%
#       addCircleMarkers(data = plot_data,
#                        lng = ~long_j,
#                        lat = ~lat_j,
#                        radius = 4,
#                        color = "black",
#                        weight = 1,
#                        fillColor = "blue",
#                        fillOpacity = 0.8,
#                        stroke = TRUE,
#                        # label = lapply(paste0(
#                        #   "<b>Location:</b> ", plot_data$location, ", ", plot_data$country, "<br>",
#                        #   "<b>Year:</b> ", plot_data$year, "<br>",
#                        #   "<b>Event:</b> ", plot_data$event, "<br>",
#                        #   "<b>Reported Disease:</b> ", plot_data$disease, "<br>",
#                        #   "<b>Disease of Concern:</b> ", plot_data$concern, "<br>",
#                        #   "<b>Species:</b> ", plot_data$species),
#                        #   HTML),
#                        layerId = ~paste0(lat_j, "_", long_j)) 
#       # addLegend(
#       #   position = "bottomright",
#       #   pal = pal,
#       #   values = world_counts$n_events,
#       #   title = "# of Observations",
#       #   opacity = 1
#       # )
#   })
#   
# #   observe({
# #     colors <- marker_colors()
# #     color_by <- input$color_by
# #     
# #     proxy <- leafletProxy("map")
# #     proxy %>% clearMarkers() %>% clearControls()
# #     
# #     if (color_by == "none") {
# #       proxy %>% clearShapes()
# #       
# #       country_counts <- plot_data %>% 
# #       count(country) %>% 
# #         rename(n_events = n)
# #       
# #       world_counts <- world %>% 
# #         left_join(country_counts, by = c("geounit" = "country")) %>% 
# #         mutate(n_events = replace_na(n_events, 0))
# #       
# #       pal <- colorNumeric(
# #         palette = plasma(100),
# #         domain = world_counts$n_events,
# #         na.color = "#999999"
# #       )
# #       
# #       proxy %>% 
# #         addPolygons(
# #           data = world_counts,
# #           fillColor = ~pal(n_events),
# #           fillOpacity = 0.7,
# #           color = "white",
# #           weight = 1,
# #           label = lapply(paste0(
# #             "<b>", world_counts$geounit, "</b><br>",
# #             "Observations: ", world_counts$n_events),
# #             HTML)
# #         ) %>% 
# #         addLegend(
# #           position = "bottomright",
# #           pal = pal,
# #           values = world_counts$n_events,
# #           title = "# of Observations",
# #           opacity = 1
# #         ) 
# #         
# #     } else {
# #       proxy %>% clearShapes()
# #       
# #       proxy %>% 
# #         addPolygons(
# #           data = world,
# #           color = "white",
# #           weight = 1,
# #           fillOpacity = 0.1
# #         )
# #     
# #     proxy %>% 
# #       addCircleMarkers(data = plot_data,
# #                        lng = ~long_j,
# #                        lat = ~lat_j,
# #                        radius = 4,
# #                        color = "black",
# #                        weight = 1,
# #                        fillColor = colors,
# #                        fillOpacity = 0.8,
# #                        stroke = TRUE,
# #                        label = lapply(paste0(
# #                          "<b>Location:</b> ", plot_data$location, ", ", plot_data$country, "<br>",
# #                          "<b>Year:</b> ", plot_data$year, "<br>",
# #                          "<b>Event:</b> ", plot_data$event, "<br>",
# #                          "<b>Reported Disease:</b> ", plot_data$disease, "<br>",
# #                          "<b>Disease of Concern:</b> ", plot_data$concern, "<br>",
# #                          "<b>Species:</b> ", plot_data$species),
# #                          HTML),
# #                        layerId = ~paste0(lat_j, "_", long_j))
# #     
# #     # if (!is.null(color_by) && color_by != "none") {
# #       
# #       family_colors <- c(
# #         "Aedes" = "#0D0887FF",
# #         "Anopheles" = "#4C02A1FF",
# #         "Culex" = "#A92395FF",
# #         "Culicoides" = "#CC4678FF",
# #         "Ochlerotatus" = "#E56B5DFF",
# #         "Blackflies" = "#F89441FF",
# #         "Sandflies" = "#FDC328FF",
# #         "Unknown" = "#999999"
# #       )
# #       
# #       col <- case_when(
# #         color_by == "species" ~ "vector_family",
# #         color_by == "disease" ~ "primary_disease",
# #         TRUE ~ color_by
# #       )
# #       values <- plot_data[[col]]
# #       values[is.na(values) | trimws(values) == ""] <- "Unknown"
# #       
# #       # palette <- colorFactor(
# #       #   palette = rainbow(length(unique(values))),
# #       #   domain = unique(values)
# #       # )
# #       
# #       if (color_by == "species") {
# #         palette <- colorFactor(
# #           palette = family_colors,
# #           domain = names(family_colors)
# #         ) 
# #       } else {
# #         unique_vals <- unique(values)
# #         palette <- colorFactor(
# #           palette = plasma(length(unique_vals)),
# #           domain = unique_vals
# #         )
# #       }
# #       
# #       proxy %>% 
# #         addLegend(
# #           position = "bottomright",
# #           pal = palette,
# #           values = values,
# #           title = switch(color_by,
# #                          "event" = "Event Type",
# #                          "disease" = "Disease",
# #                          "species" = "Vector Family"),
# #           opacity = 1
# #         )
# #     }
# #   })
# #   
# #   output$heatmap_disease <- renderPlot({
# #     heat_data <- plot_data %>% 
# #       filter(!is.na(event), !is.na(vector_family),
# #              nchar(trimws(event)) > 0) %>% 
# #       count(event, vector_family) %>% 
# #       complete(event, vector_family, fill = list(n = 0))
# #     
# #     ggplot(heat_data, aes(x = vector_family, y = event, fill = n)) +
# #       geom_tile(color = "lightgray") +
# #       geom_text(aes(label = ifelse(n == 0, "", n)), color = "gray", size = 10) +
# #       scale_fill_gradientn(
# #         colors = c("gray80", plasma(100)), 
# #         values = scales::rescale(c(0, 0.001, 1)),
# #         name = "Count") +
# #       labs(
# #         title = "Natural Events vs. Vector Family",
# #         x = "Vector Family",
# #         y = "Event Type"
# #       ) +
# #       theme_minimal() +
# #       theme(
# #         axis.title.x = element_text(size = 15),
# #         axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
# #         axis.title.y = element_text(size = 15),
# #         axis.text.y = element_text(size = 15),
# #         plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
# #         panel.grid = element_blank()
# #       )
# #   })
# #   
# #   output$heatmap_species <- renderPlot({
# #     heat_data <- plot_data %>% 
# #       filter(!is.na(event), !is.na(primary_disease),
# #              nchar(trimws(event)) > 0,
# #              nchar(trimws(primary_disease)) > 0) %>% 
# #       count(event, primary_disease) %>% 
# #       complete(event, primary_disease, fill = list(n = 0))
# #     
# #     ggplot(heat_data, aes(x = primary_disease, y = event, fill = n)) +
# #       geom_tile(color = "lightgray") +
# #       geom_text(aes(label = ifelse(n == 0, "", n)), color = "gray", size = 10) +
# #       scale_fill_gradientn(
# #         colors = c("gray80", plasma(100)), 
# #         values = scales::rescale(c(0, 0.001, 1)),
# #         name = "Count") +
# #       labs(
# #         title = "Natural Events vs. Disease",
# #         x = "Disease",
# #         y = "Event Type"
# #       ) +
# #       theme_minimal() +
# #       theme(
# #         axis.title.x = element_text(size = 15),
# #         axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
# #         axis.title.y = element_text(size = 15),
# #         axis.text.y = element_text(size = 15),
# #         plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
# #         panel.grid = element_blank()
# #       )
# #   })
# #   
# #     # Zoom for region selections
# #     observeEvent(input$region_select, {
# # 
# #       req(nchar(trimws(input$region_select)) > 0)
# # 
# #       selected_region <- world %>%
# #         filter(continent == input$region_select) %>%
# #         st_transform(4326)
# # 
# #       if (nrow(selected_region) > 0) {
# #         bbox <- st_bbox(selected_region)
# # 
# #         xmin <- as.numeric(bbox["xmin"])
# #         ymin <- as.numeric(bbox["ymin"])
# #         xmax <- as.numeric(bbox["xmax"])
# #         ymax <- as.numeric(bbox["ymax"])
# # 
# #         leafletProxy("map") %>%
# #           flyToBounds(
# #             # lng1 = bbox["xmin"],
# #             # lat1 = bbox["ymin"],
# #             # lng2 = bbox["xmax"],
# #             # lat2 = bbox["ymax"],
# #             lng1 = xmin,
# #             lat1 = ymin,
# #             lng2 = xmax,
# #             lat2 = ymax,
# #             options = list(padding = c(50, 50))
# #           )
# #       }
# # 
# #     })
# # 
# #     # Zoom for country selections
# #     observeEvent(input$country_select, {
# # 
# #       req(nchar(trimws(input$country_select)) > 0)
# # 
# #       selected_country <- world %>%
# #         filter(geounit == input$country_select) %>%
# #         st_transform(4326)
# # 
# #       if (nrow(selected_country) > 0) {
# #         bbox <- st_bbox(selected_country)
# # 
# #         xmin <- as.numeric(bbox["xmin"])
# #         ymin <- as.numeric(bbox["ymin"])
# #         xmax <- as.numeric(bbox["xmax"])
# #         ymax <- as.numeric(bbox["ymax"])
# # 
# #         leafletProxy("map") %>%
# #           flyToBounds(
# #             # lng1 = bbox["xmin"],
# #             # lat1 = bbox["ymin"],
# #             # lng2 = bbox["xmax"],
# #             # lat2 = bbox["ymax"],
# #             lng1 = xmin,
# #             lat1 = ymin,
# #             lng2 = xmax,
# #             lat2 = ymax,
# #             options = list(padding = c(50, 50))
# #           )
# #       }
# # 
# #     })
# #   
# #   observeEvent(input$map_marker_click, {
# #     click <- input$map_marker_click
# #     print("click detected")
# #     print(paste("lat:", click$lat, "lng:", click$lng))
# #     
# #     
# #     # Identify the row by matching the lat/long of the clicked marker
# #     row <- locations %>% 
# #       filter(abs(lat_j - click$lat) < 0.0001, abs(long_j - click$lng) < 0.0001) %>% 
# #       slice(1)
# #     print(paste("rows found:", nrow(row)))
# #     print(row)
# #     
# #     
# #       output$click_info <- renderUI({
# #         print("inside renderUI")
# # 
# #         tryCatch({   
# #         
# #           safe <- function(x) {
# #             x <- as.character(x)
# #             # x <- ifelse(is.na(x) | trimws(x) == "" | x == "NA", "Not reported", x)
# #             x <- gsub('"', '', x)   # remove embedded quotes
# #             x <- gsub("'", "", x)   # remove embedded single quotes
# #             x
# #           }
# #         
# #         source_display <- if (is.na(row$link) || trimws(row$link) == "" || 
# #                               !grepl("^https?://", trimws(row$link))) {
# #           tags$td(style = "padding: 5px;", "No source available")
# #         } else {
# #           tags$td(style = "padding: 5px;",
# #                   tags$a(href = trimws(row$link),
# #                          target = "_blank",
# #                          "Website Link"))
# #         }
# #         
# #         div(
# #           style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd;",
# #           h3(paste0(row$title)),
# #           tags$table(
# #             style = "width: 100%; font-size: 16px;",
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Location:"),
# #               tags$td(style = "padding: 5px;", row$location, ", ", row$country)
# #             ),
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Year:"),
# #               tags$td(style = "padding: 5px;", row$year)
# #             ),
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px;", "Natural Event:"),
# #               tags$td(style = "padding: 5px;", row$event)
# #             ),
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px;", "Reported Disease:"),
# #               tags$td(style = "padding: 5px;", row$disease)
# #             ),
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px;", "Disease of Concern:"),
# #               tags$td(style = "padding: 5px;", row$concern)
# #             ),
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px;", "Vector Species:"),
# #               tags$td(style = "padding: 5px;", row$species)
# #             ),
# #             tags$tr(
# #               tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Source:"),
# #               tags$td(style = "padding: 5px;",
# #                       tags$a(href = row$link,
# #                              target = "_blank",
# #                              "Source Website Link")
# #               )
# #             )
# #           )
# #         )
# #           
# #       }, error = function(e) {
# #         div(
# #           style = "padding: 15px; background-color: #fff3cd; border-radius: 8px; border: 1px solid #ffc107;",
# #           h4("Could not load details for this marker."),
# #           p(paste("Error:", e$message))
# #         )
# #       })
# #   })
# # })
# # }
# }
#   
# shinyApp(ui, server)


#   tabPanel("Reported Events Map",
#       
#       titlePanel("Selection Panel"),
#       
#       sidebarLayout(
#         sidebarPanel(
#           
#           # Region selection panel
#           selectInput(
#             inputId = "region_select",
#             label = "Select a Region",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # Country selection panel
#           selectInput(
#             inputId = "country_select",
#             label = "Select a Country",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # City selection panel
#           conditionalPanel(
#             condition = "input.country_select != ''",
#             selectInput(
#               inputId = "city_select",
#               label = "Select a City",
#               choices = NULL,
#               selected = NULL
#             )
#           ),
#           
#           # Date range selection panel
#           selectInput(
#             inputId = "date_select",
#             label = "Select a Date Range",
#             choices = c(
#               "All Decades" = "",
#               "1990-2000" = "1990",
#               "2000-2010" = "2000",
#               "2010-2020" = "2010",
#               "2020-Present" = "2020"
#             ),
#             selected = ""
#           ),
#           
#           hr(),
#           
#           # Event type selection
#           selectInput(
#             inputId = "event_select",
#             label = "Select a Natural Event",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # Disease selection
#           selectInput(
#             inputId = "disease_select",
#             label = "Select a Disease",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # Mosquito selection
#           selectInput(
#             inputId = "species_select",
#             label = "Select a Vector Species",
#             choices = c(
#               "Select a species..." = "")),
#           
#           actionButton("reset_btn", "Reset View")
#           
#         ),
#         
#         # Single comparisons map
#         mainPanel(
#           leafletOutput("map", height = 600),
#           hr(),
#           uiOutput("click_info")
#         )
#       ),
#                  
#   tabPanel("Systems Map"),
#   
#   )               
# )
# 
# server <- function(input, output, session) {
#   
#   # Continent filter
#   # observeEvent(TRUE, once = TRUE, {
#   #   region_choices <- locations %>% 
#   #     filter(!is.na(continent), nchar(trimws(continent)) > 0) %>% 
#   #     pull(continent) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   region_choices <- c("Select a Region..." = "", region_choices)
#   #   
#   #   updateSelectInput(
#   #     session,
#   #     "region_select",
#   #     choices = region_choices
#   #   )
#   # })
#   # 
#   # # Country filter
#   # observeEvent(TRUE, once = TRUE, {
#   #   country_choices <- locations %>% 
#   #   filter(!is.na(country), nchar(trimws(country)) > 0) %>% 
#   #     pull(country) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   country_choices <- c("Select a Country..." = "", country_choices)
#   #   
#   #   updateSelectInput(
#   #     session,
#   #     "country_select",
#   #     choices = country_choices
#   #   )
#   # })
#   
#   observe({
#     region_choices <- locations %>% 
#       filter(!is.na(continent), nchar(trimws(continent)) > 0) %>% 
#       pull(continent) %>% 
#       unique() %>% 
#       sort()
#     
#     updateSelectInput(
#       session,
#       "region_select",
#       choices = c("Select a Region..." = "", region_choices)
#     )
#   })
#   
#   observeEvent(input$region_select, {
#     # req(input$region_select != "")
#     freezeReactiveValue(input, "country_select")
#     freezeReactiveValue(input, "city_select")
#     updateSelectInput(session, "country_select", selected = "")
#     updateSelectInput(session, "city_select", selected = "")
#   }, ignoreInit = TRUE)
#   # 
#   observeEvent(input$country_select, {
#     # req(input$country_select != "")
#     # match_continent <- locations %>%
#     #   filter(country == input$country_select) %>%
#     #   pull(continent) %>%
#     #   first()
# # 
#     updateSelectInput(session, "city_select", selected = "")
#   }, ignoreInit = TRUE)
#   
#   # observeEvent(input$region_select, {
#   #   if (is.null(input$region_select) || input$region_select == "") {
#   #     df_filtered <- locations
#   #   } else {
#   #     df_filtered <- locations %>% filter(continent == input$region_select)
#   #     updateSelectInput(session, "country_select", selected = "")
#   #     updateSelectInput(session, "city_select", selected = "")
#   #   }
#   # 
#   # country_choices <- df_filtered %>%
#   #   filter(!is.na(country), nchar(trimws(country)) > 0) %>%
#   #   pull(country) %>%
#   #   unique() %>%
#   #   sort()
#   # 
#   # updateSelectInput(
#   #   session,
#   #   "country_select",
#   #   choices = c("Select a Country..." = "", country_choices),
#   #   selected = input$country_select
#   # )
#   # })
#   
#   observe({
#     # Determine country list based on region
#     if (is.null(input$region_select) || input$region_select == "") {
#       df_temp <- locations 
#     } else {
#       df_temp <- locations %>% filter(continent == input$region_select)
#     }
#     
#     c_choices <- df_temp %>%
#       filter(!is.na(country), nchar(trimws(country)) > 0) %>%
#       pull(country) %>% unique() %>% sort()
#     
#     updateSelectInput(session, "country_select",
#                       choices = c("Select a Country..." = "", c_choices),
#                       selected = input$country_select)
#   })
#   
#   # observe({
#   #   df_for_countries <- if (is.null(input$region_select) || input$region_select == "") {
#   #     locations
#   # } else {
#   #   locations %>% filter(continent == input$region_select)
#   # }
#   # 
#   # c_choices <- df_for_countries %>% 
#   #   filter(!is.na(country), nchar(trimws(country)) > 0) %>% 
#   #   pull(country) %>% unique() %>% sort()
#   # 
#   # updateSelectInput(session, "country_select",
#   #                   choices = c("Select a Country..." = "", c_choices),
#   #                   selected = input$country_select)
#   # })
#   
#   # City filter
#   # observeEvent(input$country_select, {
#   #   req(input$country_select != "")
#   #   
#   #   city_choices <- locations %>% 
#   #     filter(grepl(input$country_select, country)) %>% 
#   #     pull(location) %>% 
#   #     sort()
#   #   
#   #   # city_choices <- c("Select a City..." = "", city_choices)
#   #   
#   #   updateSelectInput(
#   #     session,
#   #     "city_select",
#   #     choices = c("Select a City..." = "", city_choices))
#   # })
#   
#   # active_selection <- reactiveVal("none")
#   
#   # Reactive filter for determining options in variable categories below
#   # filtered_locations <- reactive({
#   #   result <- locations
#   #   
#   #   if(!is.null(input$region_select) && input$region_select != ""){
#   #     result <- result %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   if(!is.null(input$country_select) && input$country_select != ""){
#   #     result <- result %>% filter(grepl(input$country_select, country))
#   #   }
#   #   
#   #   if(!is.null(input$date_select) && input$date_select != "") {
#   #     decade_start <- as.numeric(input$date_select)
#   #     decade_end <- if(decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#   #     
#   #     result <- result %>% 
#   #       filter(year >= decade_start & year < decade_end)
#   #   }
#   #   
#   #   result
#   # })
#   
#   #ORIGINAL MAP FILTER
#   # map_filter <- reactive({
#   #   
#   #   result <- locations %>% 
#   #     filter(!is.na(lat_j), !is.na(long_j))
#   #   
#   #   if (!is.null(input$region_select) && input$region_select != "") {
#   #     result <- result %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   if (!is.null(input$country_select) && input$country_select != "") {
#   #     result <- result %>% filter(grepl(input$country_select, country))
#   #   }
#   #   
#   #   if (!is.null(input$city_select) && input$city_select != "") {
#   #     result <- result %>% filter(location == input$city_select)
#   #   }
#   #   
#   #   if (!is.null(input$date_select) && input$date_select != "") {
#   #     decade_start <- as.numeric(input$date_select)
#   #     decade_end <- if (decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#   #     result <- result %>% 
#   #       filter(as.numeric(year) >= decade_start & as.numeric(year) < decade_end)
#   #   }
#   #   
#   #   if (!is.null(input$event_select) && input$event_select != "") {
#   #     result <- result %>% filter(event == input$event_select)
#   #   }
#   #   
#   #   if (!is.null(input$disease_select) && input$disease_select != "") {
#   #     result <- result %>% filter(disease == input$disease_select)
#   #   }
#   #   
#   #   if (!is.null(input$species_select) && input$species_select != "") {
#   #     result <- result %>% filter(grepl(input$species_select, species))
#   #   }
#   #   
#   #   result
#   #   
#   # }) 
#   
#   map_filter <- reactive({
#     result <- locations %>% 
#       filter(!is.na(lat_j), !is.na(long_j))
#     
#     if (!is.null(input$region_select) && input$region_select != "") {
#       result <- result %>% filter(continent == input$region_select)
#     }
#     
#     if (!is.null(input$country_select) && input$country_select != "") {
#       result <- result %>% filter(country == input$country_select)
#     }
#     
#     if (!is.null(input$city_select) && input$city_select != "") {
#       result <- result %>% filter(location == input$city_select)
#     }
#     
#     if (!is.null(input$date_select) && input$date_select != "") {
#       decade_start <- as.numeric(input$date_select)
#       decade_end <- if (decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#       result <- result %>%
#         filter(as.numeric(year) >= decade_start & as.numeric(year) < decade_end)
#     } 
#     
#     if (!is.null(input$event_select) && input$event_select != "") {
#       result <- result %>% filter(event == input$event_select)
#     }
# 
#     if (!is.null(input$disease_select) && input$disease_select != "") {
#       result <- result %>% filter(disease == input$disease_select)
#     }
# 
#     if (!is.null(input$species_select) && input$species_select != "") {
#       result <- result %>% filter(grepl(input$species_select, species))
#     }
# 
#     return(result)
#   })
#   
#   # Species
#   # observeEvent(map_filter(), {
#   #   species_choices <- map_filter() %>% 
#   #     filter(!is.na(species), nchar(trimws(species)) > 0) %>% 
#   #     pull(species) %>% 
#   #     strsplit(",") %>% 
#   #     unlist() %>% 
#   #     trimws() %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   updateSelectInput(session, "species_select",
#   #                     choices = c("Select a Species..." = "", species_choices),
#   #                     selected = "")
#   # })
#   
#   # FILTER OBSERVE
#   # observe({
#   #   data <- map_filter()
#   # 
#   #   updateSelectInput(session, "event_select",
#   #                     choices = c("All Events" = "", sort(unique(data$event))))
#   #   updateSelectInput(session, "disease_select",
#   #                     choices = c("All Diseases" = "", sort(unique(data$disease))))
#   # 
#   #   spec <- data$species %>% strsplit(",") %>% unlist() %>% trimws() %>% unique() %>% sort()
#   #   updateSelectInput(session, "species_select",
#   #                     choices = c("All Species" = "", spec))
#   # })
# 
#   # Events
#   # observeEvent(map_filter(), {
#   #   event_choices <- map_filter() %>% 
#   #     filter(!is.na(event), nchar(trimws(event)) > 0) %>% 
#   #     pull(event) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   updateSelectInput(session, "event_select",
#   #                     choices = c("Select an Event..." = "", event_choices),
#   #                     selected = "")
#   #   
#   # })
#   # 
#   # # Diseases
#   # observeEvent(map_filter(), {
#   #   disease_choices <- map_filter() %>% 
#   #     filter(!is.na(disease), nchar(trimws(disease)) > 0) %>% 
#   #     pull(disease) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   updateSelectInput(session, "disease_select",
#   #                     choices = c("Select a Disease..." = "", disease_choices),
#   #                     selected = "")
#   #   
#   # })
#   
#   # map_filter <- reactive({
#   #   
#   #   result <- locations %>% 
#   #     filter(!is.na(lat_j), !is.na(long_j))
#   #   
#   #   if (!is.null(input$region_select) && input$region_select != "") {
#   #     result <- result %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   if (!is.null(input$country_select) && input$country_select != "") {
#   #     result <- result %>% filter(grepl(input$country_select, country))
#   #   }
#   #   
#   #   if (!is.null(input$city_select) && input$city_select != "") {
#   #     result <- result %>% filter(location == input$city_select)
#   #   }
#   #   
#   #   if (!is.null(input$date_select) && input$date_select != "") {
#   #     decade_start <- as.numeric(input$date_select)
#   #     decade_end <- if (decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#   #     result <- result %>% 
#   #       filter(as.numeric(year) >= decade_start & as.numeric(year) < decade_end)
#   #   }
#   #   
#   #   if (!is.null(input$event_select) && input$event_select != "") {
#   #     result <- result %>% filter(event == input$event_select)
#   #   }
#   # 
#   #   if (!is.null(input$disease_select) && input$disease_select != "") {
#   #     result <- result %>% filter(disease == input$disease_select)
#   #   }
#   # 
#   #   if (!is.null(input$species_select) && input$species_select != "") {
#   #     result <- result %>% filter(grepl(input$species_select, species))
#   #   }
#   #   
#   #   result
#   #   
#   # }) 
#   
#   
#   # output$map <- renderLeaflet({
#   #   leaflet() %>% 
#   #     addTiles() %>% 
#   #     # addPolygons(data = world,
#   #     #             color = "orange",
#   #     #             weight = 1,
#   #     #             fillOpacity = 0.3) %>% 
#   #     # addCircleMarkers(data = map_filter(),
#   #     #                  lng = ~long_j,
#   #     #                  lat = ~lat_j,
#   #     #                  radius = 4,
#   #     #                  color = "blue",
#   #     #                  fillOpacity = 0.8,
#   #     #                  popup = ~paste(location))
#   #     addCircleMarkers(lng = -80.19, lat = 25.77, popup = "test")
#   # })
#   
#   # output$map <- renderLeaflet({
#   #   data <- map_filter()
#   #   
#   #   leaflet() %>%
#   #     addTiles() %>%
#   #     setView(lng = 0, lat = 30, zoom = 1.75) %>% 
#   #     # addPolygons(data = world,
#   #     #             color = "orange",
#   #     #             weight = 1,
#   #     #             fillOpacity = 0.3) %>%
#   #     addCircleMarkers(data = data,
#   #                      lng = ~long_j,
#   #                      lat = ~lat_j,
#   #                      radius = 4,
#   #                      color = "blue",
#   #                      fillOpacity = 0.8,
#   #                      label = lapply(paste0(
#   #                        "<b>Location:</b> ", data$location, ", ", data$country, "<br>",
#   #                        "<b>Year:</b> ", data$year, "<br>",
#   #                        "<b>Event:</b> ", data$event, "<br>",
#   #                        "<b>Reported Disease:</b> ", data$disease, "<br>",
#   #                        "<b>Disease of Concern:</b> ", data$concern, "<br>",
#   #                        "<b>Species:</b> ", data$species),
#   #                                     HTML))
#   # })
#   
#   # output$map <- renderLeaflet({
#   #   leaflet() %>%
#   #     addTiles() %>%
#   #     setView(lng = 0, lat = 30, zoom = 1.75)
#   # })
#   
#   # map_data <- reactiveVal(locations %>% filter(!is.na(lat_j), !is.na(long_j)))
#   # 
#   # observe({
#   #   map_data(map_filter())
#   # })
#   
#   # observe({
#   #   data <- map_filter()
#   #   
#   #   proxy <- leafletProxy("map") 
#   #   proxy %>% clearMarkers()
#   #     
#   #   if (nrow(data) > 0) {
#   #     proxy %>% 
#   #       addCircleMarkers(data = data,
#   #                        lng = ~long_j,
#   #                        lat = ~lat_j,
#   #                        radius = 4,
#   #                        color = "blue",
#   #                        fillOpacity = 0.8,
#   #                        label = lapply(paste0(
#   #                          "<b>Location:</b> ", data$location, ", ", data$country, "<br>",
#   #                          "<b>Year:</b> ", data$year, "<br>",
#   #                          "<b>Event:</b> ", data$event, "<br>",
#   #                          "<b>Reported Disease:</b> ", data$disease, "<br>",
#   #                          "<b>Disease of Concern:</b> ", data$concern, "<br>",
#   #                          "<b>Species:</b> ", data$species),
#   #                          HTML)
#   #       )
#   #   }  
#   # })
#   
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       setView(lng = 0, lat = 30, zoom = 1.75)
#   })
#   
#   observe({
#     data <- map_filter()
#     proxy <- leafletProxy("map") 
#     proxy %>% clearMarkers()
#     
#     if (nrow(data) > 0) {
#       proxy %>% 
#       # clearMarkers() %>%
#       addCircleMarkers(
#         data = data,
#         lng = ~long_j, lat = ~lat_j,
#         radius = 4, color = "blue", fillOpacity = 0.8,
#         label = ~paste0(location, ", ", country)
#       )
#     }
#   })
#   
# #   ORIGINAL OBSERVE BLOCK
#   # observe({
#   #   data <- map_filter()
#   #   
#   #   leafletProxy("map") %>%
#   #     clearMarkers() %>%
#   #     addCircleMarkers(data = data,
#   #                      lng = ~long_j,
#   #                      lat = ~lat_j,
#   #                      radius = 4,
#   #                      color = "blue",
#   #                      fillOpacity = 0.8,
#   #                      label = lapply(paste0(
#   #                        "<b>Location:</b> ", data$location, ", ", data$country, "<br>",
#   #                        "<b>Year:</b> ", data$year, "<br>",
#   #                        "<b>Event:</b> ", data$event, "<br>",
#   #                        "<b>Reported Disease:</b> ", data$disease, "<br>",
#   #                        "<b>Disease of Concern:</b> ", data$concern, "<br>",
#   #                        "<b>Species:</b> ", data$species),
#   #                        HTML))
#   # })
#   
#   # Zoom for region selections
#   observeEvent(input$region_select, {
#     
#     req(nchar(trimws(input$region_select)) > 0)
#     
#     selected_region <- world %>% 
#       filter(continent == input$region_select) %>% 
#       st_transform(4326)
#     
#     if (nrow(selected_region) > 0) {
#       bbox <- st_bbox(selected_region)
#       
#       xmin <- as.numeric(bbox["xmin"])
#       ymin <- as.numeric(bbox["ymin"])
#       xmax <- as.numeric(bbox["xmax"])
#       ymax <- as.numeric(bbox["ymax"])
#       
#       leafletProxy("map") %>%
#         flyToBounds(
#           # lng1 = bbox["xmin"],
#           # lat1 = bbox["ymin"],
#           # lng2 = bbox["xmax"],
#           # lat2 = bbox["ymax"],
#           lng1 = xmin,
#           lat1 = ymin,
#           lng2 = xmax,
#           lat2 = ymax,
#           options = list(padding = c(50, 50))
#         )
#     }
#     
#   })
#   
#   # Zoom for country selections
#   observeEvent(input$country_select, {
#     
#     req(nchar(trimws(input$country_select)) > 0)
#     
#     selected_country <- world %>% 
#       filter(geounit == input$country_select) %>% 
#       st_transform(4326)
#     
#     if (nrow(selected_country) > 0) {
#       bbox <- st_bbox(selected_country)
#       
#       xmin <- as.numeric(bbox["xmin"])
#       ymin <- as.numeric(bbox["ymin"])
#       xmax <- as.numeric(bbox["xmax"])
#       ymax <- as.numeric(bbox["ymax"])
#       
#       leafletProxy("map") %>%
#         flyToBounds(
#           # lng1 = bbox["xmin"],
#           # lat1 = bbox["ymin"],
#           # lng2 = bbox["xmax"],
#           # lat2 = bbox["ymax"],
#           lng1 = xmin,
#           lat1 = ymin,
#           lng2 = xmax,
#           lat2 = ymax,
#           options = list(padding = c(50, 50))
#         )
#     }
#     
#   })
#   
#   clicked_info <- reactiveVal(NULL)
#   
#   observeEvent(input$map_marker_click, {
#     click <- input$map_marker_click
#     
#     clicked_row <- map_filter() %>% 
#       filter(abs(lat_j - click$lat) < 0.0001,
#              abs(long_j - click$lng) < 0.0001) %>% 
#       slice(1)
#     
#     clicked_info(clicked_row)
#   })
#   
#   output$click_info <- renderUI({
#     req(clicked_info())
#     row <- clicked_info()
#     
#     div(
#       style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd;",
#       h3(paste0(row$title)),
#       tags$table(
#         style = "width: 100%; font-size: 16px;",
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Location:"),
#           tags$td(style = "padding: 5px;", row$location, ", ", row$country)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Year:"),
#           tags$td(style = "padding: 5px;", row$year)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Natural Event:"),
#           tags$td(style = "padding: 5px;", row$event)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Reported Disease:"),
#           tags$td(style = "padding: 5px;", row$disease)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Disease of Concern:"),
#           tags$td(style = "padding: 5px;", row$concern)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Vector Species:"),
#           tags$td(style = "padding: 5px;", row$species)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Source:"),
#           tags$td(style = "padding: 5px;",
#                   tags$a(href = row$link,
#                          target = "_blank",
#                          row$link)
#           )
#         ),
#       )
#     )
#   })
#   
#   observeEvent(input$reset_btn, {
#     # 1. Freeze everything so the map doesn't flicker while resetting
#     freezeReactiveValue(input, "region_select")
#     freezeReactiveValue(input, "country_select")
#     freezeReactiveValue(input, "city_select")
#     freezeReactiveValue(input, "event_select")
#     freezeReactiveValue(input, "disease_select")
#     freezeReactiveValue(input, "species_select")
#     
#     # 2. Hard reset all inputs
#     updateSelectInput(session, "region_select", selected = "")
#     updateSelectInput(session, "country_select", selected = "")
#     updateSelectInput(session, "city_select", selected = "")
#     updateSelectInput(session, "date_select", selected = "")
#     updateSelectInput(session, "event_select", selected = "")
#     updateSelectInput(session, "disease_select", selected = "")
#     updateSelectInput(session, "species_select", selected = "")
#     
#     # 3. Clear the click info and reset zoom
#     clicked_info(NULL)
#     leafletProxy("map") %>% flyTo(lng = 0, lat = 30, zoom = 1.75)
#   })
#   
# }

# shinyApp(ui = ui, server = server)








