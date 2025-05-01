library(shiny)
library(lubridate)
library(data.table)
library(dplyr)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(tmap)

# gridid_to_coords <- function(ID, n_cols = 12) {
#   row <- ceiling(ID / n_cols)
#   col <- ID - (row - 1) * n_cols
#   return(list(row = row, col = col))
# }
# complete_grid <- data.frame(ID = as.numeric(1:144))  # 12x12 = 144 cells
# complete_grid <- complete_grid %>%
#   mutate(
#     coords = lapply(ID, gridid_to_coords),
#     row = sapply(coords, function(x) x$row),
#     col = sapply(coords, function(x) x$col)
#   ) %>%
#   select(-coords)
# radar <- left_join(complete_grid, radar, by = "ID")
# available_dates <- sort(unique(radar$Date[!is.na(radar$Date)]))
# mostobs <- radar %>% 
#   group_by(ID, Radar, Date) %>% 
#   unique() %>%
#   ungroup() %>%
#   group_by(Date) %>% 
#   summarise(count = n()) %>% 
#   arrange()

### testdate
date = as.Date("22/07/2021", format = "%d/%m/%Y")
###

radar <- fread("C:/Users/rocon/OneDrive/Documents/insect-radar-AES/data/radar_abundance_110325.csv") %>%
  filter(!is.na(Latitude)) %>%
  filter(Height == "dBZ_500") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(ID = as.numeric(gsub("GridID_", "", ID))) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) 

radar_f <- radar %>% 
  filter(Date == date)

UK <- ne_countries(country = "united kingdom", type = "countries", scale = "medium")
UK <- UK$geometry



# Create the map with tmap
tm_shape(UK) +
  tm_fill(col = "lightgrey") +
  tm_borders() +
  tm_shape(radar_f) +
  tm_dots(col = "N", palette = "viridis", style = "cont", legend.show = F) +
  tm_layout(frame = FALSE)

# ## leaflet version
# 
# library(leaflet)
# 
# leaflet(data = radar_f, options = leafletOptions(zoomControl = T, dragging = T)) %>%  #width = range(radar_f$Latitude), height = range(radar_f$Longitude))) %>%
#   addPolygons(data = UK, fillColor = "grey", fillOpacity = 1, color = "white", weight = 1) %>%
#   addCircleMarkers(
#     ~Longitude, ~Latitude,
#     color = ~colorNumeric("viridis", N)(N),  # Continuous color based on N
#     radius = 4,
#     popup = ~paste0("ID: ", ID, "<br>N: ", N)  # Popup displaying ID and N
#   ) # %>%
#   #setView(lng = mean(radar_f$Longitude), lat = mean(radar_f$Latitude), zoom = 5)


ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Nowcast v0"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
      selectInput("DatesMerge",
                  "Select Date:",
                  choices = available_dates,
                  selected = available_dates[length(available_dates) %/% 2]), 
      
      
    ),
    
    mainPanel(
      # Increase the size of the plot by specifying height and width
      plotOutput("PlotRadar", height = "700px", width = "1000px"))
    
  )
))



server <- shinyServer(function(input, output) {
  
  output$PlotRadar <- renderPlot({
    
    #Create the data
    
    date <- input$DatesMerge
    radar_f <- radar %>% filter(Date == date)
 
    # draw the histogram with the specified number of bins
      tm_shape(UK) +
        tm_fill(col = "lightgrey") +
        tm_borders() +
        tm_shape(radar_f) +
        tm_dots(col = "N", palette = "viridis", style = "cont", legend.show = F) +
        tm_layout(frame = FALSE)
      
      
  })
})
shinyApp(ui, server)














