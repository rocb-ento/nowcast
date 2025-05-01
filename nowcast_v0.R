library(shiny)
library(lubridate)
library(data.table)
library(dplyr)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(tmap)

### testdate
#date = as.Date("22/07/2021", format = "%d/%m/%Y")
###
# =============================== #
#  load data + maps 
# =============================== #
cvp_covariates <- st_read("C:/Users/rocon/OneDrive/Documents/insect-radar-AES/data/covariate_data/cvp_covariates.geojson") %>%
  as.data.frame() %>% 
  select(col_lat, col_long)

cvp_locations <- cvp_covariates %>%
  st_as_sf(., coords = c("col_long", "col_lat"), crs = 4326) 

radar <- fread("C:/Users/rocon/OneDrive/Documents/insect-radar-AES/data/radar_abundance_110325.csv") %>%
  filter(!is.na(Latitude)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(ID = as.numeric(gsub("GridID_", "", ID))) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) 
# radar_f <- radar %>% 
#   filter(Date == date)
UK <- ne_countries(country = "united kingdom", type = "countries", scale = "medium")
UK <- UK$geometry

# =============================== #
# 
# =============================== #
available_dates <- sort(unique(format(radar$Date, format = "%m-%d")))
available_dates <- as.numeric(gsub("-", ".", available_dates))
library(shinyWidgets)
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Nowcast v0"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
      sliderInput("height", label = "Height",
                  min = 100, max = 1700, step = 200, value = 500),
      sliderInput("year", label = "Year",
                  min = 2012, max = 2022, step = 1, value = 2016),
      sliderTextInput("date", label = "Date",
                      choices = available_dates, 
                      selected = available_dates[1], 
                      grid = TRUE) # Optional grid for visual guidance
    ),
    
      
     # sliderInput("date", label = "Date", min = min(available_dates), max = max(available_dates), value = available_dates[100])),
    
    
    mainPanel(
      # Increase the size of the plot by specifying height and width
      plotOutput("PlotRadar", height = "1000px", width = "500px"))
   )
  )
)


# year = 2022
# doy = 4.12
# date = as.Date(paste0(year, ".", doy), format = "%Y.%m.%d")
# height = "500"
server <- shinyServer(function(input, output) {
  
  # cache the base map
  base_map <- tm_shape(UK) +
    tm_fill(col = "darkgrey") +
    tm_borders() +
   # tm_layout(frame = FALSE) +
    tm_shape(cvp_locations) +
    tm_dots(col = "lightgrey")
  
  output$PlotRadar <- renderPlot({
    height = paste0("dBZ_",input$height)
    date = as.Date(paste0(input$year, ".", input$date), format = "%Y.%m.%d")
    radar_f <- radar %>% 
      filter(Height == height) %>%
      filter(Date == date)
    
    if ((nrow(radar_f)) > 0) {
    base_map +
      tm_shape(radar_f) +
      tm_dots(col = "N", palette = "viridis", style = "cont", legend.show = F)
      } else {
        base_map
      }
  })
})


shinyApp(ui, server)











