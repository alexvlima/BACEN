###########################
#####                 #####
#####     SHINY APP   #####
#####                 #####
###########################
###################
### BIBLIOTECAS ###
###################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(lubridate)
library(plotly)


####################
### SHINY DEPLOY ###
####################

ui <- dashboardPage(skin = 'blue', 
  dashboardHeader(title = 'Boletim Focus'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Intro', tabName = 'intro', icon = icon('info')),
      menuItem('PIB', tabName = 'pib', icon = icon('dollar')),
      menuItem('Infla\u00e7\u00e3o', tabName = 'inflacao', icon = icon('line-chart')),
      menuItem('C\u00e2mbio', tabName = 'cambio', icon = icon('money')),
      menuItem('Selic', tabName = 'selic', icon = icon('line-chart')),
      menuItem('Sobre', tabName = 'sobre', icon = icon('question'))
      
    ) 
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              h3('Introdu\u00e7\u00e3o'),
              h5('Este dashboard foi constru\u00eddo a partir dos dados disponibilizados nas s\u00e9ries temporais do Banco Central. Os dados correspondem as informa\u00e7\u00f5es do Boletim Focus.
                 '),
              h3('Boletim Focus'),
              h5('Semanalmente .....')),
      tabItem(tabName = 'pib',
              fluidRow(
                box(
                  plotlyOutput('Graf_PIB1')
                  ),
                box(
                  plotlyOutput('Graf_PIB2')
                )
                ),
              fluidRow(
                box(
                  title = h3(HTML('<center>Indicador</center>')),
                  selectInput('indic_pib', label = h4('Indicador:'),
                              choices = list('Producao industrial' = 'Produ\u00e7\u00e3o industrial', 
                                             'PIB Agropecuaria' = 'PIB Agropecu\u00e1ria', 
                                             'PIB Industrial' = 'PIB Industrial', 
                                             'PIB Servicos' = 'PIB Servi\u00e7os', 
                                             'PIB Total' = 'PIB Total'),
                              selected = 'PIB Total')
                ),
                box(
                  title = h3(HTML('<center>Filtros</center>')),
                  dateRangeInput(inputId = 'data_pib',
                                 label = h4('Selecione o per\u00edodo:'),
                                 format = 'mm/yyyy',
                                 language='pt-BR',
                                 min = '2018-01-01',
                                 max = Sys.Date(),
                                 start = '2018-01-01',
                                 end = Sys.Date(),
                                 startview = 'year',
                                 separator = '-'),
                  numericInput(inputId = 'num_pib', 
                               label = h4('Expectativa do Mercado para o Ano:'), 
                               value = year(Sys.Date()), 
                               min = year(Sys.Date()), 
                               max = year(Sys.Date())+4),
                  selectInput('metrica_pib', label = h4('M\u00e9trica:'), 
                              choices = list('Media' = 'mean', 'Mediana' = 'median', 'Minimo' = 'min', 'Maximo' = 'max', 'Desvio Padrao' = 'sd', 'Coeficiente de Variacao' = 'coefvar'),
                              selected = 'mean')
                )
                )
              ),
      tabItem(tabName = 'inflacao'),
      tabItem(tabName = 'cambio'),
      tabItem(tabName = 'selic'),
      tabItem(tabName = 'sobre',
              h3('Equipe'),
              h5(HTML('- Alexandre Lima 
                  <br>- Ananda S\u00e1
                  <br>- Aretha Zarlenga
                  <br>- Luiz Hissashi
                  <br>- J\u00e9ssica Lakiss
                  <br>- Karina Souza
                  <br>- Pedro Souza
                  <br>- Tomaz Carrijo'))
              )
    )
  )
)


server <- function(input, output) {
  
  dates_pib <- reactiveValues()
  observe({
    dates_pib$SelectedDates <- c(as.character(format(input$data_pib[1],format = '%m/%Y')),
                                 as.character(format(input$data_pib[2],format = '%m/%Y')))
  })
  
  base_pib1 <- reactive({
    pib %>%
      select(indic,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_pib,
             metric == input$metrica_pib,
             date >= input$data_pib[1] & date <= input$data_pib[2])
  })
  
  output$Graf_PIB1 <- 
    renderPlotly({
      Base_pib1 <- base_pib1() %>% filter(date == max(date))
      plot_ly(Base_pib1, 
              x = ~reference_year, y=~value) %>%
        layout(title = 'Crescimento Esperado do PIB',
               xaxis = list(title = ''),
               yaxis = list(title = '% a.a')) %>%
        add_trace(hoverinfo = 'y')
  
    })

  base_pib2 <- reactive({
    pib %>%
      select(indic,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_pib,
             reference_year==input$num_pib,
             metric == input$metrica_pib,
             date >= input$data_pib[1] & date <= input$data_pib[2])
  })
  
  output$Graf_PIB2 <- 
    renderPlotly({
      Base_pib2 <- base_pib2() %>% filter(reference_year==input$num_pib)
      plot_ly(Base_pib2, 
              x = ~date, y=~value) %>%
        layout(title = paste0('Crescimento Esperado para ',input$num_pib),
               xaxis = list(title = ''),
               yaxis = list(title = '% a.a')) %>%
        add_trace(hoverinfo = 'y')
      
    })
}
  

shinyApp(ui, server)
 
