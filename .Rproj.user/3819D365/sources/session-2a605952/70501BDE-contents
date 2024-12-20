library(shiny)
library(shinyjs)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(DT)
library(dplyr)

###############
# Sample Data #
###############
set.seed(123)  # Ensure reproducibility
sample_data <- data.frame(
  id = 1:100,
  scientific_name = sample(c("Quercus rubra", "Acer saccharum", "Pinus strobus"), 100, replace = TRUE),
  diameter_base_height = runif(100, 5, 50),
  height = runif(100, 10, 100),
  stems = sample(1:5, 100, replace = TRUE),
  overhead_utilities = sample(c("Yes", "No"), 100, replace = TRUE),
  land_use = sample(c("Residential", "Commercial", "Park"), 100, replace = TRUE),
  condition = rep(c("Excellent", "Very Good", "Good", "Poor", "Critical"), times = 20),
  overall_benefits_dollar_value = runif(100, 100, 1000),
  neighborhood = rep(c("Neighborhood A", "Neighborhood B", "Neighborhood C"), length.out = 100),
  latitude = runif(100, 40.43, 40.47),
  longitude = runif(100, -79.96, -79.90)
)

neighborhoods_all <- unique(sample_data$neighborhood)
conditions_all <- unique(sample_data$condition)
diameter_maxmin <- range(sample_data$diameter_base_height)
stem_maxmin <- range(sample_data$stems)

##########
#   UI   #
##########
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(".blurred-panel {
      background: rgba(255, 255, 255, 0.4);
      backdrop-filter: blur(10px);
      border-radius: 10px;
      padding: 15px;
    }
    #tree_map {
      height: calc(100vh - 90px) !important;
    }"))
  ),
  navbarPage("Pittsburgh Trees", 
             theme = shinytheme("cerulean"),
             tabPanel("Map", 
                      div(class = "mymap",
                          leafletOutput("tree_map", width = "100%", height = "100%"),
                          absolutePanel(
                            id = "filters",
                            class = "blurred-panel",
                            fixed = TRUE,
                            draggable = TRUE, 
                            top = 85, 
                            left = "auto", 
                            right = 40, 
                            bottom = "auto",
                            width = 330, 
                            height = "auto",
                            
                            h3("Filters:"),
                            checkboxGroupInput("neighborhood_select",
                                               "Neighborhoods:",
                                               choices = neighborhoods_all,
                                               selected = neighborhoods_all),
                            checkboxGroupInput("condition_select",
                                               "Tree Condition:",
                                               choices = conditions_all,
                                               selected = conditions_all),
                            sliderInput("diameter_range",
                                        "Diameter Base Height (ft):",
                                        min = diameter_maxmin[1],
                                        max = diameter_maxmin[2],
                                        value = diameter_maxmin),
                            sliderInput("stems_range",
                                        "Stem Count:",
                                        min = stem_maxmin[1],
                                        max = stem_maxmin[2],
                                        value = stem_maxmin),
                            actionButton('reset_filters',
                                         'Reset Filters',
                                         icon = icon("refresh"))
                          )
                      )
             ), 
             tabPanel("Table", 
                      fluidPage(
                        downloadButton('download_tree_data', "Download Data"),
                        tags$div(tags$br()), 
                        DT::dataTableOutput("tree_table"))
             ) 
  )
)

##########
# Server #
##########
server <- function(input, output, session) {
  # Reactive filtered data
  tree_data <- reactive({
    req(input$neighborhood_select, input$condition_select)  # Ensure inputs are available
    
    selected_neighborhoods <- if (is.null(input$neighborhood_select) || length(input$neighborhood_select) == 0) {
      neighborhoods_all
    } else {
      input$neighborhood_select
    }
    
    selected_conditions <- if (is.null(input$condition_select) || length(input$condition_select) == 0) {
      conditions_all
    } else {
      input$condition_select
    }
    
    sample_data %>%
      filter(
        neighborhood %in% selected_neighborhoods,
        condition %in% selected_conditions,
        diameter_base_height >= input$diameter_range[1],
        diameter_base_height <= input$diameter_range[2],
        stems >= input$stems_range[1],
        stems <= input$stems_range[2]
      )
  })
  
  # Render the map
  output$tree_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 40.45, lng = -79.95, zoom = 13)  # Default view
  })
  
  # Update map with filtered data
  observe({
    filtered_data <- tree_data()
    proxy <- leafletProxy("tree_map") %>%
      clearMarkers() %>%
      clearPopups()
    
    if (nrow(filtered_data) > 0) {
      proxy %>%
        addCircleMarkers(
          lng = filtered_data$longitude,
          lat = filtered_data$latitude,
          radius = 5,
          color = "blue",
          fillOpacity = 0.7,
          popup = paste0(
            "<b>Tree ID: </b>", filtered_data$id, "<br/>",
            "<b>Scientific Name: </b>", filtered_data$scientific_name, "<br/>",
            "<b>Condition: </b>", filtered_data$condition, "<br/>",
            "<b>Diameter: </b>", round(filtered_data$diameter_base_height, 2)
          )
        )
    } else {
      proxy %>%
        addPopups(
          lng = -79.95,
          lat = 40.45,
          popup = "No data available for the selected filters."
        )
    }
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "neighborhood_select", selected = neighborhoods_all)
    updateCheckboxGroupInput(session, "condition_select", selected = conditions_all)
    updateSliderInput(session, "diameter_range", value = diameter_maxmin)
    updateSliderInput(session, "stems_range", value = stem_maxmin)
  })
  
  # Render the data table
  output$tree_table <- DT::renderDataTable({
    DT::datatable(tree_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Download filtered data
  output$download_tree_data <- downloadHandler(
    filename = function() {
      paste("tree-data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tree_data(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
