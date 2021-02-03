## app.R ##

## Dash board para el data set 'mtcars'

library(shiny)
library(shinydashboard)
library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Equipo 19"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Frecuencia de Goles", tabName = "Dashboard", icon = icon("dashboard")),
                    menuItem("Probabilidades", tabName = "img", icon = icon("file-picture-o")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                    menuItem("Factores de Ganancia", tabName = "img2", icon = icon("file-picture-o"))
                    
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Histograma
                    tabItem(tabName = "Dashboard",
                            sidebarPanel(
                                p("Resultados"), 
                                selectInput("x", "Selecciona",
                                            choices = c('home', 'away'))
                            ),
                            
                            mainPanel(
                                
                                tabsetPanel(            
                                    tabPanel("Goles",   
                                             (textOutput("output_text")), 
                                             plotOutput("output_plot")
                                    )
                                )
                            )
                    ),
                    tabItem(tabName = "img",
                            fluidRow(
                                titlePanel(h3("Probabilidad Marginal: Equipo de Casa")),
                                img( src = "ProbMarg_EC.png", 
                                     height = 350, width = 550)
                            ),
                            fluidRow(
                                titlePanel(h3("Porbabilidad Marginal: Equipo Visitante")),
                                img( src = "ProbMarg_EV.png", 
                                     height = 350, width = 550)
                            ),
                            fluidRow(
                                titlePanel(h3("Mapa de calor de probabilidad conjunta")),
                                img( src = "Heatmap_PC.png", 
                                     height = 350, width = 550)
                            )
                    ),
                    
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table de partidos")),
                                dataTableOutput ("data_table")
                            )
                    ),
                    
                    tabItem(tabName = "img2",
                            fluidRow(
                                titlePanel(h3("Factor de Ganancias Mínimo")),
                                img( src = "FG_Min.png", 
                                     height = 350, width = 550)
                            ),
                            fluidRow(
                                titlePanel(h3("Factor de Ganancias Mínimo")),
                                img( src = "FG_Max.png", 
                                     height = 350, width = 550)
                            )
                )
            )
        )
    )
)

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    
    #Gráfica de barras    
    Home <-as.data.frame(table(factor(md$home.score, levels = 0:10)))
    Away <- as.data.frame(table(factor(md$away.score, levels = 0:10)))
    names(Home) <- c("Goles", "Frecuencia")
    names(Away) <- c("Goles", "Frecuencia")
    output$output_plot <- renderPlot(
        if(input$x == 'home'){
            plot(Home)} else 
            {plot(Away)}
    )
    
    
    #Data Table
 
    output$data_table <- renderDataTable( {md}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
}


shinyApp(ui, server)