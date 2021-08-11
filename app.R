#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(rsconnect)
library(dplyr)
library(highcharter)
library(shinythemes)

#source("./global.R",encoding = "utf-8")


Base2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSVGsfo-35qnY13X4EC8-vxOrilvfGQrvGP7MrGKWyEW89MteLL_Xa9yRheW6yNjP1Npn01dK8H6tm/pub?output=csv")
Base2 <- Base2[,-6]

Localidades <- count(Base2,Localidad)

Estratos <- count(Base2,Estrato)

Nueva <- count(Base2,Localidad, Estrato)


full <- read.csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")
names(full)[2] = "Pais"
full$Pais <- recode(full$Pais, "United States" = "United States of America")
full$Pais <- recode(full$Pais, "Democratic Republic of Congo" = "Democratic Republic of the Congo")
full$Pais <- recode(full$Pais, "Tanzania" = "United Republic of Tanzania")
full$Pais <- recode(full$Pais, "Congo" = "Republic of Congo")


WORLD <- get_data_from_map(download_map_data("custom/world-palestine"))

Mundo <- WORLD%>%
    select(Pais = "name")


Total <- merge(full,Mundo, by="Pais" )

Total_Mundo = Total%>%
    filter(date == "2020-11-27")%>%
    select(date,Pais,total_cases )




mapdata <- get_data_from_map(download_map_data("countries/co/co-all"))

Colombia <- mapdata%>%
    select(Departamento = "name")%>%
    mutate(x=c(1:34))





# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("superhero"),
                #shinythemes::themeSelector()
                
                # Application title
                titlePanel("Dashboard Google form"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 10)
                    ),
                    
                    ## Otra forma de poner los tags    
                    #withTags({
                    #div(class="header", checked=NA,
                    #p("Le gustaria ver tal cosa "),
                    #a(href="link de destino", "Click Here!")
                    #)
                    #})
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        
                        tags$div(class="header", checked=NA,
                                 tags$p("Le gustaria ver mi repositorio de github?" ),
                                 tags$a(href="https://github.com/Jomapime", "Da click aca!")),
                        
                        tabsetPanel(
                            tabPanel("Histograma", highchartOutput("distPlot"),br(),br(),
                                     verbatimTextOutput("summary")), 
                            tabPanel("Localidades", highchartOutput("Dinamico"), br(), br(),
                                     highchartOutput("Barras")),
                            tabPanel("Regresion", highchartOutput("Regre")),
                            tabPanel("Base de datos",dataTableOutput("Tablaa")),
                            tabPanel("Mapa", highchartOutput("Mapaa")),
                            tabPanel("Mapa_C", highchartOutput("MapaC"))
                        )
                        
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderHighchart({
        # generate bins based on input$bins from ui.R
        bins <- seq(min(Base2$Edad), max(Base2$Edad), length.out = input$bins + 1)
        
        hchart(Base2$Edad, breaks = bins, color = "Green",showInLegend = FALSE) %>%
            hc_title(text = "Histograma Edades")%>%
            hc_yAxis(
                title = list(text = "Frecuencia Relativa"))%>%
            hc_xAxis(
                title = list(text = "Edad"),
                min = 10, max=60)%>%
            hc_add_theme(hc_theme_538())
        
        
    })
    
    output$summary <- renderPrint({
        Base3 = Base2
        Base3$Localidad = as.factor(Base2$Localidad)
        Base3$Estrato = as.factor(Base2$Estrato)
        summary(Base3)
    })
    
    # crear grafico de barras y pie dinamicos
    
    output$Dinamico <- renderHighchart({
        
        highchart() %>%
            # Data
            hc_add_series(Localidades, 
                          type = "column",
                          hcaes(x = Localidad, y = n), name = "Cantidad") %>%
            hc_add_series(Estratos,
                          type = "pie", 
                          hcaes(name = Estrato, y = n), name = "Cantidad") %>% 
            hc_add_theme(hc_theme_ffx()) %>%
            # Options for each type of series
            hc_plotOptions(
                series = list(
                    showInLegend = FALSE,
                    pointFormat = "{point.y}%"
                ),
                column = list(
                    colorByPoint = TRUE
                ),
                pie = list(
                    colorByPoint = TRUE, center = c('30%', '10%'),
                    size = 120, dataLabels = list(enabled = FALSE)
                )) %>%
            # Axis
            hc_yAxis(
                title = list(text = "¿Donde viven los encuestados?"),
                labels = list(format = "{value}")
            ) %>%
            hc_xAxis(categories = Localidades$Localidad) %>%
            # Titles and credits
            hc_title(
                text = "Grafico de barras con localidades y grafico de torta con estratos"
                
            ) %>%
            hc_subtitle(text = "Condiciones donde viven") %>%
            hc_credits(
                enabled = TRUE, text = "Fuente: De los deseos",
                style = list(fontSize = "10px")
            )    
    })
    
    output$Barras <- renderHighchart({
        
        hchart(Nueva, "column", hcaes(x = Localidad, y = n, group = Estrato)) %>%
            hc_chart(type = "column",
                     options3d = list(
                         enabled = TRUE, 
                         beta = 15,
                         alpha = 15))%>%
            hc_add_theme(hc_theme_alone())
        
    })
    
    
    
    output$Tablaa <- renderDataTable({
        Base2
    })
    
    output$Mapaa <- renderHighchart({
        
        hcmap("custom/world-palestine", data = Total_Mundo, value = "total_cases",
              nullColor = "#f70707",
              joinBy = c("name", "Pais"), 
              dataLabels = list(enabled = TRUE, format = '{point.name}'),
              tooltip = list(valueSuffix = " Contagiados"))%>%
            hc_mapNavigation(enabled = TRUE)%>%
            hc_title(text = "<i>Mapa dinamico mundial</i> - <b>Jose Pinzon</b>",
                     margin = 20, align = "center",
                     style = list(color = "#08338F", useHTML = TRUE))%>%
            hc_subtitle(text = "Casos totales de COVID - 27/11/2020",
                        align = "center",
                        style = list(color = "#0C5C9E", fontWeight = "bold"))%>%
            hc_chart(borderColor = "#08338F",
                     borderRadius = 10,
                     borderWidth = 2)%>%
            hc_add_theme(hc_theme_ffx())%>%
            hc_credits(enabled = T, text = "Fuente: Mayami me lo confirmo")%>%
            hc_caption(text = "No se tiene informacion de los paises con color <i>ROJO</i>.",
                       style = list(fontSize = "10px"))
        
        
        
    })
    
    output$MapaC <- renderHighchart({
        
        hcmap("countries/co/co-all", data = Colombia, value = "x",
              joinBy = c("name", "Departamento"),
              dataLabels = list(enabled = F, format = '{point.name}'))%>%
            hc_mapNavigation(enabled = TRUE)%>%
            hc_title(text = "<i>Mapa dinamico de Colombia</i> - <b>Jose Pinzon</b>",
                     margin = 20, align = "center",
                     style = list(color = "Orange", useHTML = TRUE))%>%
            hc_chart(borderColor = "#08338F",
                     borderRadius = 10,
                     borderWidth = 2)%>%
            hc_colorAxis(
                stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)))%>%
            hc_add_theme(hc_theme_flatdark())
        
    })
    
    output$Regre <- renderHighchart({
        
        hchart(Base2, "scatter", hcaes(x = Altura, y = Peso, group = Localidad)) %>%
            hc_title(text = "Grafico dispersion")%>%
            hc_add_theme(hc_theme_ffx())
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
