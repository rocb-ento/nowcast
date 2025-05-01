library(shiny)
library(lubridate)
library(data.table)
library(dplyr)

radar <- fread("C:/Users/rocon/OneDrive/Documents/insect-radar-AES/data/radar_abundance_110325.csv") %>%
  filter(radar == "Chenies") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(ID = as.numeric(gsub("GridID_", "", ID)))
gridid_to_coords <- function(ID, n_cols = 12) {
  row <- ceiling(ID / n_cols)
  col <- ID - (row - 1) * n_cols
  return(list(row = row, col = col))
}
complete_grid <- data.frame(ID = as.numeric(1:144))  # 12x12 = 144 cells
complete_grid <- complete_grid %>%
  mutate(
    coords = lapply(ID, gridid_to_coords),
    row = sapply(coords, function(x) x$row),
    col = sapply(coords, function(x) x$col)
  ) %>%
  select(-coords)
radar <- left_join(complete_grid, radar, by = "ID")
available_dates <- sort(unique(radar$Date[!is.na(radar$Date)]))

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Nowcast v0"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
      selectInput("DatesMerge",
                  "Select Date:",
                  choices = available_dates,
                  selected = available_dates[length(available_dates) %/% 2]), # Select middle date as default
    ),
    mainPanel(
      plotOutput("PlotRadar"))
    
  )
))


server <- shinyServer(function(input, output) {
  
  output$PlotRadar <- renderPlot({
    
    #Create the data
    
    date <- input$DatesMerge
    radar_f <- radar %>% filter(Date == date)
    if ( (nrow(radar_f) > 1) ){
    # draw the histogram with the specified number of bins
      ggplot(radar_f, aes(x = col, y = row, fill = N)) +
        geom_tile(color = "white", linewidth = 0.1) +  # Add thin white borders between cells
        scale_fill_viridis_c(name = "Abundance", na.value = "grey90") +
        scale_y_reverse() +  # Reverse Y-axis to have row 1 at the top
        scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
        scale_y_continuous(breaks = 1:12, limits = c(1, 12)) +
        coord_equal() +  # Make cells square
        theme_minimal() +
        labs(title = "",
             x = "grid X", 
             y = "grid Y") +
        theme(panel.grid = element_blank())
      }
  })
})
shinyApp(ui, server)


## TD



















