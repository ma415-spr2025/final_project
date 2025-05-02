#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# demographics <- need data
# climate_data <- need data 

# UI
ui <- navbarPage(
  title = "São Miguel Island, Azores",
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabPanel("Overview",
           fluidPage(
             titlePanel("Welcome to São Miguel Island"),
             fluidRow(
               column(6, img(src = "sao_miguel.jpg", width = "100%", alt = "São Miguel Island")),
               column(6,
                      h4("About the Island"),
                      p("São Miguel is the largest island in the Azores archipelago, located in the North Atlantic Ocean. Known as the 'Green Island', it features volcanic landscapes, crater lakes, and lush pastures."),
                      tableOutput("demoTable")
               )
             )
           )
  ),
  
  tabPanel("Maps",
           fluidPage(
             titlePanel("Geographic Location"),
             leafletOutput("map", height = 600)
           )
  ),
  
  tabPanel("Climate",
           fluidPage(
             titlePanel("Climate Data"),
             plotlyOutput("climatePlot"),
             br(),
             h5("São Miguel has a mild, oceanic climate with rainfall spread throughout the year and pleasant summer temperatures.")
           )
  ),
  
  tabPanel("Data Tables",
           fluidPage(
             titlePanel("More Stats and Demographics"),
             DTOutput("dataTable")
           )
  ),
  
  tabPanel("Geology & Formation",
           fluidPage(
             titlePanel("Geological Origins"),
             p("São Miguel was formed by volcanic activity, with the oldest parts dating back around 4 million years. Its topography features calderas, fumaroles, and thermal springs."),
             img(src = "map_sao_miguel.png", width = "100%", alt = "Geological Map")
           )
  )
)

# Server
server <- function(input, output, session) {
  output$demoTable <- renderTable({
    demographics
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -25.5, lat = 37.8, zoom = 8) %>%
      addMarkers(lng = -25.5, lat = 37.8, popup = "São Miguel Island")
  })
  
  output$climatePlot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = Month)) +
      geom_bar(aes(y = Rainfall), stat = "identity", fill = "skyblue") +
      geom_line(aes(y = Temperature * 5, group = 1), color = "red", size = 1) +  # Scaled to match
      scale_y_continuous(
        name = "Rainfall (mm)",
        sec.axis = sec_axis(~./5, name = "Temperature (°C)")
      ) +
      labs(title = "Monthly Climate Overview") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$dataTable <- renderDT({
    datatable(demographics, options = list(pageLength = 5, dom = 'tp'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)



# Define UI for application that draws a histogram
#ui <- fluidPage(

    # Application title
    #titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
       # sidebarPanel(
      #      sliderInput("bins",
     #                   "Number of bins:",
    #                    min = 1,
   #                     max = 50,
  #                      value = 30)
 #       ),
#
        # Show a plot of the generated distribution
    #    mainPanel(
   #        plotOutput("distPlot")
  #      )
 #   )
#)

# Define server logic required to draw a histogram
#server <- function(input, output) {
#
  #  output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
     #   x    <- faithful[, 2]
      #  bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
      #  hist(x, breaks = bins, col = 'darkgray', border = 'white',
       #      xlab = 'Waiting time to next eruption (in mins)',
        #     main = 'Histogram of waiting times')
    #})
#}

# Run the application 
#shinyApp(ui = ui, server = server)
