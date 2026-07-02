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
    lat_j = jitter(Lat, amount = 0.5),
    long_j = jitter(Long, amount = 0.5)
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
                   "Articles Map",
                   titlePanel(
                     HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Pandemic Prediction in the Space Age: Use of Earth Observation (EOS) Data</div>")
                     ),
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

                             div(
                               tags$b("Directions"),
                               tags$ul(
                                 tags$li("Select a variable below to view a color-coded version of the map"),
                                 tags$li("Use the map to zoom in and click on the markers for more details about each study"),
                                 tags$li("Studies with the same location are clustered together, so it will be necessary to zoom in on them to be able to click on each"),
                                 tags$li("The cluster of studies inside of the orange circle in the center of the map correspond to those with a global or multinational scale which could not be assigned to one specific country"),
                                 tags$li("Use the 'Reporting Checklist' below to see which necessary characteristics each study reports in its methodology"),
                                 tags$li("Use the 'Overall Reporting Summary' below to see the total percentages of studies that report each necessary characteristic")
                               )
                             ),
                             
                             div(
                               tags$b("Be sure to review the 'About' tab to learn more about the foundations and purpose of this study")
                             ),
                              
                             br(),
                             
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
                   titlePanel(
                     HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Charts and Graphs</div>")
                   ),
                   
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
                           # "Terrestrial Variables" = "tervar",
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
                 ),
                 
                 tabPanel(
                   "About",
                   titlePanel(
                     HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>About the Study</div>")
                   ),
                   sidebarLayout(
                     sidebarPanel(
                       h4("General Concepts"),
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
        map %>%
          addCircles(
            lng = 0,
            lat = 0,
            radius = 500000,
            color = "orange",
            weight = 10,
            fill = TRUE,
            fillOpacity = 0,
            stroke = TRUE,
            options = pathOptions(interactive = FALSE)
          ) %>% 
        addCircleMarkers(
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
      addCircles(
        lng = 0,
        lat = 0,
        radius = 500000,
        color = "orange",
        weight = 10,
        fill = TRUE,
        fillOpacity = 0,
        stroke = TRUE,
        options = pathOptions(interactive = FALSE)
      ) %>% 
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
      list(label = "Geophysical Variables Reported: ", col = "georep")
      # list(label = "Terrestrial Variables Reported: ", col = "terrep")
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
      list(label = "Geophysical Variables Reported", col = "georep")
      # list(label = "Terrestrial Variables Reported", col = "terrep")
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
                    # "tervar" = "Terrestrial Variables",
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
                      # "tervar" = "Terrestrial Variables",
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
                      # "tervar" = "Terrestrial Variables",
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
                            "georep" = "Geophysical Variables"),
                            # "terrep" = "Terrestrial Variables"),
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







