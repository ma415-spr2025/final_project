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
library(DT)
library(ggplot2)
library(plotly)
library(slickR)
library(shinyBS)
library(wordcloud2)

# === Data ===

islands <- data.frame(
  Island = c("Sao Miguel", "Pico", "Terceira", "Sao Jorge", "Faial", "Flores", "Santa Maria", "Graciosa", "Corvo"), 
  Area = c(759, 446, 403, 246, 173, 143, 97, 62, 17),
  Population = c(133295, 13883, 53244, 8373, 14334, 3428, 5408, 4091, 384),
  Dist_of_GDP = c(58.2, 5.0, 21.5, 3.3, 6.2, 1.3, 2.8, 1.5, 0.2)
)

climate_data <- data.frame(
  Month = month.abb,
  Temperature = c(52.4, 52.3, 56.5, 59.3, 63.8, 68.7, 70.7, 71.8, 69.5, 65.3, 57.7, 54.1),
  Rainfall = c(62, 55, 60, 56, 37, 9, 2, 4, 25, 73, 72, 76)
)

# === UI ===
ui <- navbarPage(
  title = tags$div(
    img(src = "azores_flag.svg", class = "flag-icon"),
    tags$span("São Miguel Island, Azores", style = "vertical-align:middle; font-weight:bold; font-size: 20px;")
  ),
  theme = shinytheme("cerulean"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tags$style(HTML("
  .azores-flag-navbar {
    position: absolute;
    right: 20px;
    top: 10px;
    height: 30px;
  }
")),
  tags$img(src = "azores_flag.svg", class = "azores-flag-navbar"),
  tabPanel("Overview",
           fluidPage(
             titlePanel("Welcome to São Miguel Island"),
             fluidRow(
               column(6,
                      img(src = "sao_miguel.jpg", width = "100%"),
                      br(),
                      div(class = "card", style = "padding: 15px; margin-top: 10px;",
                          h4("Lush Landscapes and Volcanic Beauty"),
                          p("São Miguel’s natural beauty is unmatched — crater lakes like Lagoa das Sete Cidades, thermal hot springs in Furnas, and dramatic coastlines make it a nature-lover's paradise.The islands are committed to sustainable tourism, and over 25% of their land is designated as 
                            Protected Areas that are set aside for conservation. The Azores began implementing Marine Protected Areas in the 1980s, the first being established in Faial, and now there are over 110,000 square kilometers of Marine Protected Areas across the Islands. The goal of these protected areas is to conserve marine biodiversity, habitats, and ecosystems.")
                      ),
                      br(),
               ),
               column(6,
                div(class = "card",
                      h4("About the Island"),
                      p("São Miguel is the largest island in the Azores archipelago, located in the North Atlantic Ocean. Known as the 'Green Island', it features volcanic landscapes, crater lakes, and lush pastures."),
                      p("The Azores are a collection of Islands located in the Atlantic Ocean, 930 miles off the coast of Lisbon. The archipelago is made up of nine major islands and eight small Formigas, spanning 373 miles."),
                      p("The islands were discovered by the Portuguese in 1427 during the Age of Exploration, and since then have played an important role as a layover point for ships moving between Europe and North America. 
                        However, a number of hypogea have been found on the islands of Corvo, Santa Maria, and Terceira by an archaelogist, indicating a potential human presence pre-dating the Portuguese."),
                      p("The islands are considered an autonomous region of Portugual, and their economy functions mostly on eco-tourism!" ),
                      ),
                div(class = "card",
                bsCollapse(
                        bsCollapsePanel("Fun Facts About São Miguel",
                                        p("More cows than people — over 140,000!"),
                                        p("Europe's only tea plantations are here."),
                                        p("Pineapples grow in greenhouses."),
                                        p("Meals are cooked in volcanic ground heat!")
                        )
                      )
               )
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
             h4("Rainfall (mm)"),
             plotlyOutput("rainfallPlot"),
             br(),
             h4("Temperature (°F)"),
             plotlyOutput("temperaturePlot")
           )
  ),
  
  tabPanel("Gallery",
           titlePanel("Photo Gallery of São Miguel"),
           slickROutput("imageGallery", width = "80%", height = "400px")
  ),
  
  
  tabPanel("Geology",
           fluidPage(
             titlePanel("Geological Origins"),
             p("São Miguel was formed by volcanic activity along the tectonic boundaries of the Eurasian, North American, and African plates. Its landscape is shaped by calderas, hot springs, and lava fields, making it a prime example of geothermal geomorphology.
               Some of the volcanoes formed over 4 million years ago by the plates pushing against each other, and the island is home to many fault lines, as well as eight different geomorphological features. 
               These include Sete Cidades Massif, the Picos Volcanic System, the Agua de Pau Massif, and the Furnas Volcano. The last volcanic eruption took place in the 17th century, and the most famous eruption was called Fogo 2 and it occurred in 1652."),
             img(src = "map_sao_miguel.png", width = "100%")
           )
  ),
  
  tabPanel("Island Comparisons",
           titlePanel("Azores Islands Data Comparison"),
           plotlyOutput("popPlot"),
           plotlyOutput("areaPlot"),
           plotlyOutput("gdpPie"),
           plotlyOutput("scatterPlot"),
           plotlyOutput("bubblePlot")
  ),
  
  tabPanel("Sources",
           fluidPage(
             titlePanel("Sources"),
             p(a("History of São Miguel – Azores.com", href = "https://azores.com/azores/islands/sao-miguel/history-2", target = "_blank")),
             p(a("São Miguel Climate Data – Climate-Data.org", href = "https://en.climate-data.org/europe/portugal/sao-miguel/sao-miguel-290075/", target = "_blank")),
             p(a("Azores – Wikipedia", href = "https://en.wikipedia.org/wiki/Azores", target = "_blank")),
             p(a("Azores and the EU – European Parliament", href = "https://www.europarl.europa.eu/RegData/etudes/BRIE/2017/601971/IPOL_BRI(2017)601971_EN.pdf", target = "_blank")),
             p(a("São Miguel Island – Wikipedia", href = "https://en.wikipedia.org/wiki/S%C3%A3o_Miguel_Island", target = "_blank")),
             p(a("Marine Protected Areas - CCMAR", href = "https://web.archive.org/web/20231205041627/https://www.ccmar.ualg.pt/en/page/marine-protected-areas", target = "_blank"))
           )
  ),
  
  
  tags$footer(
    class = "footer",
    style = "text-align: center; padding: 10px; background-color: #e8f5f9;",
    tags$img(src = "azores_flag.svg", class = "flag-icon"),
    tags$p("Built with Shiny", style = "margin: 5px; font-size: 0.9em; color: #555;")
  )
)

# === Server ===
server <- function(input, output, session) {
  output$dataTable <- renderDT({ datatable(demographics) })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -25.5, lat = 37.8, zoom = 9) %>%
      addMarkers(lng = -25.78, lat = 37.87, 
                 popup = "<b>Sete Cidades</b><br><img src='sete_cidades.jpg' width='200px'>") %>%
      addMarkers(lng = -25.48, lat = 37.77, 
                 popup = "<b>Lagoa do Fogo</b><br><img src='lagoa_fogo.jpg' width='200px'>") %>%
      addMarkers(lng = -25.67, lat = 37.73, 
                 popup = "<b>Furnas</b><br><img src='furnas.jpg' width='200px'>")
  })
  
  output$rainfallPlot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = Month, y = Rainfall)) +
      geom_col(fill = "lightblue3") +
      labs(title = "Monthly Rainfall", y = "Rainfall (mm)", x = "Month") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$temperaturePlot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = Month, y = Temperature)) +
      geom_line(color = "darkolivegreen3", size = 1.2) +
      geom_point(color = "darkolivegreen3", size = 2) +
      labs(title = "Monthly Temperature", y = "Temperature (°F)", x = "Month") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$imageGallery <- renderSlickR({
    slickR(c("gallery1.jpg", "gallery2.jpg", "gallery3.jpg", "gallery4.jpg", "gallery5.jpg"))
  })
  
  
  output$popPlot <- renderPlotly({
    p <- ggplot(islands, aes(x = reorder(Island, -Population), y = Population)) +
      geom_col(fill = "cadetblue2") +
      labs(title = "Population by Island", x = "Island", y = "Population") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$areaPlot <- renderPlotly({
    p <- ggplot(islands, aes(x = reorder(Island, -Area), y = Area)) +
      geom_col(fill = "forestgreen") +
      labs(title = "Area by Island", x = "Island", y = "Area (km²)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$gdpPie <- renderPlotly({
    plot_ly(data = islands, labels = ~Island, values = ~Dist_of_GDP, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = "GDP Contribution by Island")
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(islands, aes(x = Area, y = Population, text = Island)) +
      geom_point(color = "deeppink2", size = 4) +
      labs(title = "Area vs Population", x = "Area (km²)", y = "Population") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$bubblePlot <- renderPlotly({
    p <- ggplot(islands, aes(x = Population, y = Dist_of_GDP, size = Area, text = Island)) +
      geom_point(alpha = 0.7, color = "orchid3") +
      labs(title = "Population vs GDP Contribution", x = "Population", y = "GDP Contribution (%)") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

# === Run App ===
shinyApp(ui, server)


