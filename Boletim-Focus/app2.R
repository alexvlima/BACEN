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

##########
### UI ###
##########

ui <- dashboardPage(skin = 'blue', 
  dashboardHeader(title = 'Boletim Focus'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Intro', tabName = 'intro', icon = icon('info')),
      menuItem('PIB', tabName = 'pib', icon = icon('dollar')),
      menuItem('Infla\u00e7\u00e3o', tabName = 'inflacao', icon = icon('line-chart')),
      menuItem('C\u00e2mbio', tabName = 'cambio', icon = icon('money')),
      menuItem('Selic', tabName = 'selic', icon = icon('line-chart')),
      menuItem('Balan\u00e7a Comercial', tabName = 'balanca_comercial', icon = icon('balance-scale')),
      menuItem('Sobre', tabName = 'sobre', icon = icon('question'))
    ) 
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              h1('Introdu\u00e7\u00e3o'),
              h3('Este dashboard foi constru\u00eddo a partir dos dados disponibilizados nas s\u00e9ries temporais do Banco Central. Os dados correspondem as informa\u00e7\u00f5es do Boletim Focus.
                 '),
              h1('Boletim Focus'),
              h3('Semanalmente, o Banco Central elabora um relat\u00f3rio com as expectativas de mercado para os principais \u00edndices econ\u00f4micos. O Departamento de Relacionamento com Investidores e Estudos Especiais (Gerin) \u00e9 o setor respons\u00e1vel pela confec\u00e7\u00e3o do relat\u00f3rio. O Focus faz parte do arcabou\u00e7o do regime monet\u00e1rio de metas de infla\u00e7\u00e3o. Seu objetivo \u00e9 monitorar a evolu\u00e7\u00e3o das expectativas de mercado, de forma a gerar subs\u00eddio para o processo decis\u00f3rio da pol\u00edtica monet\u00e1ria. 
                 ')),
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
      tabItem(tabName = 'inflacao',
              fluidRow(
                box(
                  plotlyOutput('Graf_Inflacao1')
                ),
                box(
                  plotlyOutput('Graf_Inflacao2')
                )
              ),
              fluidRow(
                box(
                  title = h3(HTML('<center>Indicador</center>')),
                  selectInput('indic_inflacao', label = h4('Indicador:'),
                                choices = list('IGP-DI' = 'IGP-DI', 
                                               'IGP-M' = 'IGP-M', 
                                               'INPC' = 'INPC',
                                               'IPA-DI' = 'IPA-DI',
                                               'IPA-M' = 'IPA-M', 
                                               'IPCA' = 'IPCA',
                                               'IPCA-15' = 'IPCA-15',
                                               'Precos administrados por contrato e monitorados' = 'Pre\u00e7os administrados por contrato e monitorados'),
                                selected = 'IPCA')
                ),
                box(
                  title = h3(HTML('<center>Filtros</center>')),
                  dateRangeInput(inputId = 'data_inflacao',
                                 label = h4('Selecione o per\u00edodo:'),
                                 format = 'mm/yyyy',
                                 language='pt-BR',
                                 min = '2018-01-01',
                                 max = Sys.Date(),
                                 start = '2018-01-01',
                                 end = Sys.Date(),
                                 startview = 'year',
                                 separator = '-'),
                  numericInput(inputId = 'num_inflacao', 
                               label = h4('Expectativa do Mercado para o Ano:'), 
                               value = year(Sys.Date()), 
                               min = year(Sys.Date()), 
                               max = year(Sys.Date())+4),
                  selectInput('metrica_inflacao', label = h4('M\u00e9trica:'), 
                              choices = list('Media' = 'mean', 'Mediana' = 'median', 'Minimo' = 'min', 'Maximo' = 'max', 'Desvio Padrao' = 'sd', 'Coeficiente de Variacao' = 'coefvar'),
                              selected = 'mean')
                )
              )
              ),
      tabItem(tabName = 'cambio',
              fluidRow(
                box(
                  plotlyOutput('Graf_Cambio1')
                ),
                box(
                  plotlyOutput('Graf_Cambio2')
                )
              ),
              fluidRow(
                box(
                  title = h3(HTML('<center>Indicador</center>')),
                  selectInput('indic_cambio', label = h4('Indicador:'),
                              choices = list('Media do ano' = 'M\u00e9dia do ano',
                                             'Fim do ano' = 'Fim do ano'),
                              selected = 'Fim do ano')
                ),
                box(
                  title = h3(HTML('<center>Filtros</center>')),
                  dateRangeInput(inputId = 'data_cambio',
                                 label = h4('Selecione o per\u00edodo:'),
                                 format = 'mm/yyyy',
                                 language='pt-BR',
                                 min = '2018-01-01',
                                 max = Sys.Date(),
                                 start = '2018-01-01',
                                 end = Sys.Date(),
                                 startview = 'year',
                                 separator = '-'),
                  numericInput(inputId = 'num_cambio', 
                               label = h4('Expectativa do Mercado para o Ano:'), 
                               value = year(Sys.Date()), 
                               min = year(Sys.Date()), 
                               max = year(Sys.Date())+4),
                  selectInput('metrica_cambio', label = h4('M\u00e9trica:'), 
                              choices = list('Media' = 'mean', 'Mediana' = 'median', 'Minimo' = 'min', 'Maximo' = 'max', 'Desvio Padrao' = 'sd', 'Coeficiente de Variacao' = 'coefvar'),
                              selected = 'mean')
                )
              )
              ),
      tabItem(tabName = 'selic',
              fluidRow(
                box(
                  plotlyOutput('Graf_Selic1')
                ),
                box(
                  plotlyOutput('Graf_Selic2')
                )
              ),
              fluidRow(
                box(
                  title = h3(HTML('<center>Indicador</center>')),
                  selectInput('indic_selic', label = h4('Indicador:'),
                              choices = list('Media do ano' = 'M\u00e9dia do ano',
                                             'Fim do ano' = 'Fim do ano'),
                              selected = 'Fim do ano')
                ),
                box(
                  title = h3(HTML('<center>Filtros</center>')),
                  dateRangeInput(inputId = 'data_selic',
                                 label = h4('Selecione o per\u00edodo:'),
                                 format = 'mm/yyyy',
                                 language='pt-BR',
                                 min = '2018-01-01',
                                 max = Sys.Date(),
                                 start = '2018-01-01',
                                 end = Sys.Date(),
                                 startview = 'year',
                                 separator = '-'),
                  numericInput(inputId = 'num_selic', 
                               label = h4('Expectativa do Mercado para o Ano:'), 
                               value = year(Sys.Date()), 
                               min = year(Sys.Date()), 
                               max = year(Sys.Date())+4),
                  selectInput('metrica_selic', label = h4('M\u00e9trica:'), 
                              choices = list('Media' = 'mean', 'Mediana' = 'median', 'Minimo' = 'min', 'Maximo' = 'max', 'Desvio Padrao' = 'sd', 'Coeficiente de Variacao' = 'coefvar'),
                              selected = 'mean')
                )
              )
              ),
      tabItem(tabName = 'balanca_comercial',
              fluidRow(
                box(
                  plotlyOutput('Graf_Balanca_Comercial1')
                ),
                box(
                  plotlyOutput('Graf_Balanca_Comercial2')
                )
              ),
              fluidRow(
                box(
                  title = h3(HTML('<center>Indicador</center>')),
                  selectInput('indic_balanca_comercial', label = h4('Indicador:'),
                              choices = list('Exportacoes' = 'Exporta\u00e7\u00f5es',
                                             'Importacoes' = 'Importa\u00e7\u00f5es',
                                             'Saldo' = 'Saldo'),
                              selected = 'Saldo')
                ),
                box(
                  title = h3(HTML('<center>Filtros</center>')),
                  dateRangeInput(inputId = 'data_balanca_comercial',
                                 label = h4('Selecione o per\u00edodo:'),
                                 format = 'mm/yyyy',
                                 language='pt-BR',
                                 min = '2018-01-01',
                                 max = Sys.Date(),
                                 start = '2018-01-01',
                                 end = Sys.Date(),
                                 startview = 'year',
                                 separator = '-'),
                  numericInput(inputId = 'num_balanca_comercial', 
                               label = h4('Expectativa do Mercado para o Ano:'), 
                               value = year(Sys.Date()), 
                               min = year(Sys.Date()), 
                               max = year(Sys.Date())+4),
                  selectInput('metrica_balanca_comercial', label = h4('M\u00e9trica:'), 
                              choices = list('Media' = 'mean', 'Mediana' = 'median', 'Minimo' = 'min', 'Maximo' = 'max', 'Desvio Padrao' = 'sd', 'Coeficiente de Variacao' = 'coefvar'),
                              selected = 'mean')
                )
              )
      ),
      tabItem(tabName = 'sobre',
              h1('Sobre'),
              h3(HTML('Esse painel foi produzido pela Unidade de Gest\u00e3o Estrat\u00e9gica do Sebrae Nacional. 
                     <br> Foi utilizada a linguagem de programa\u00e7\u00e3o R para a manipula\u00e7\u00e3o dos dados. A publica\u00e7\u00e3o do painel ocorreu por meio da biblioteca Shiny.')),
              h1('Equipe'),
              h3(HTML('- Alexandre Lima 
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


##############
### SERVER ###
##############

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
  
  dates_inflacao <- reactiveValues()
  observe({
    dates_inflacao$SelectedDates <- c(as.character(format(input$data_inflacao[1],format = '%m/%Y')),
                                      as.character(format(input$data_inflacao[2],format = '%m/%Y')))
  })
  
  base_inflacao1 <- reactive({
    inflacao %>%
      select(indic,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_inflacao,
             metric == input$metrica_inflacao,
             date >= input$data_inflacao[1] & date <= input$data_inflacao[2])
  })
  
  output$Graf_Inflacao1 <- 
    renderPlotly({
      Base_inflacao1 <- base_inflacao1() %>% filter(date == max(date))
      plot_ly(Base_inflacao1, 
              x = ~reference_year, y=~value) %>%
        layout(title = 'Taxa Esperada de Infla\u00e7\u00e3o',
               xaxis = list(title = ''),
               yaxis = list(title = '% a.a')) %>%
        add_trace(hoverinfo = 'y')
      
    })
  
  base_inflacao2 <- reactive({
    inflacao %>%
      select(indic,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_inflacao,
             reference_year==input$num_inflacao,
             metric == input$metrica_inflacao,
             date >= input$data_inflacao[1] & date <= input$data_inflacao[2])
  })
  
  output$Graf_Inflacao2 <- 
    renderPlotly({
      Base_inflacao2 <- base_inflacao2() %>% filter(reference_year==input$num_inflacao)
      plot_ly(
        Base_inflacao2, 
        x = ~date, y=~value) %>%
        layout(title = paste0('Taxa Esperada para ',input$num_inflacao),
               xaxis = list(title = ''),
               yaxis = list(title = '% a.a')) %>%
        add_trace(hoverinfo = 'y')
  })
  
  dates_cambio <- reactiveValues()
  observe({
    dates_cambio$SelectedDates <- c(as.character(format(input$data_cambio[1],format = '%m/%Y')),
                                      as.character(format(input$data_cambio[2],format = '%m/%Y')))
  })
  
  base_cambio1 <- reactive({
    cambio %>%
      select(indic_detail,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic_detail,date,reference_year)) %>%
      filter(indic_detail==input$indic_cambio,
             metric == input$metrica_cambio,
             date >= input$data_cambio[1] & date <= input$data_cambio[2])
  })
  
  output$Graf_Cambio1 <- 
    renderPlotly({
      Base_cambio1 <- base_cambio1() %>% filter(date == max(date))
      plot_ly(Base_cambio1, 
              x = ~reference_year, y=~value) %>%
        layout(title = 'Taxa Esperada do C\u00e2mbio',
               xaxis = list(title = ''),
               yaxis = list(title = 'R$ / US$')) %>%
        add_trace(hoverinfo = 'y')
      
    })
  
  base_cambio2 <- reactive({
    cambio %>%
      select(indic_detail,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic_detail,date,reference_year)) %>%
      filter(indic_detail==input$indic_cambio,
             reference_year==input$num_cambio,
             metric == input$metrica_cambio,
             date >= input$data_cambio[1] & date <= input$data_cambio[2])
  })
  
  output$Graf_Cambio2 <- 
    renderPlotly({
      Base_cambio2 <- base_cambio2() %>% filter(reference_year==input$num_cambio)
      plot_ly(
        Base_cambio2, 
        x = ~date, y=~value) %>%
        layout(title = paste0('Taxa Esperada para ',input$num_cambio),
               xaxis = list(title = ''),
               yaxis = list(title = 'R$ / US$')) %>%
        add_trace(hoverinfo = 'y')
    })
  
  dates_selic <- reactiveValues()
  observe({
    dates_selic$SelectedDates <- c(as.character(format(input$data_selic[1],format = '%m/%Y')),
                                    as.character(format(input$data_selic[2],format = '%m/%Y')))
  })
  
  base_selic1 <- reactive({
    selic %>%
      select(indic_detail,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic_detail,date,reference_year)) %>%
      filter(indic_detail==input$indic_selic,
             metric == input$metrica_selic,
             date >= input$data_selic[1] & date <= input$data_selic[2])
  })
  
  output$Graf_Selic1 <- 
    renderPlotly({
      Base_selic1 <- base_selic1() %>% filter(date == max(date))
      plot_ly(Base_selic1, 
              x = ~reference_year, y=~value) %>%
        layout(title = 'Taxa Esperada da Selic',
               xaxis = list(title = ''),
               yaxis = list(title = '% a.a')) %>%
        add_trace(hoverinfo = 'y')
      
    })
  
  base_selic2 <- reactive({
    selic %>%
      select(indic_detail,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic_detail,date,reference_year)) %>%
      filter(indic_detail==input$indic_selic,
             reference_year==input$num_selic,
             metric == input$metrica_selic,
             date >= input$data_selic[1] & date <= input$data_selic[2])
  })
  
  output$Graf_Selic2 <- 
    renderPlotly({
      Base_selic2 <- base_selic2() %>% filter(reference_year==input$num_selic)
      plot_ly(
        Base_selic2, 
        x = ~date, y=~value) %>%
        layout(title = paste0('Taxa Esperada para ',input$num_selic),
               xaxis = list(title = ''),
               yaxis = list(title = '% a.a')) %>%
        add_trace(hoverinfo = 'y')
    })
  
  dates_balanca_comercial <- reactiveValues()
  observe({
    dates_balanca_comercial$SelectedDates <- c(as.character(format(input$data_balanca_comercial[1],format = '%m/%Y')),
                                               as.character(format(input$data_balanca_comercial[2],format = '%m/%Y')))
  })
  
  base_balanca_comercial1 <- reactive({
    balanca_comercial %>%
      select(indic_detail,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic_detail,date,reference_year)) %>%
      filter(indic_detail==input$indic_balanca_comercial,
             metric == input$metrica_balanca_comercial,
             date >= input$data_balanca_comercial[1] & date <= input$data_balanca_comercial[2])
  })
  
  output$Graf_Balanca_Comercial1 <- 
    renderPlotly({
      Base_balanca_comercial1 <- base_balanca_comercial1() %>% filter(date == max(date))
      plot_ly(Base_balanca_comercial1, 
              x = ~reference_year, y=~value) %>%
        layout(title = 'Valor Esperado da Balan\u00e7a Comercial',
               xaxis = list(title = ''),
               yaxis = list(title = 'US$ (em bilh\u00f5es)')) %>%
        add_trace(hoverinfo = 'y')
      
    })
  
  base_balanca_comercial2 <- reactive({
    balanca_comercial %>%
      select(indic_detail,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic_detail,date,reference_year)) %>%
      filter(indic_detail==input$indic_balanca_comercial,
             reference_year==input$num_balanca_comercial,
             metric == input$metrica_balanca_comercial,
             date >= input$data_balanca_comercial[1] & date <= input$data_balanca_comercial[2])
  })
  
  output$Graf_Balanca_Comercial2 <- 
    renderPlotly({
      Base_balanca_comercial2 <- base_balanca_comercial2() %>% filter(reference_year==input$num_balanca_comercial)
      plot_ly(
        Base_balanca_comercial2, 
        x = ~date, y=~value) %>%
        layout(title = paste0('Valor Esperado da Balan\u00e7a Comercial para ',input$num_balanca_comercial),
               xaxis = list(title = ''),
               yaxis = list(title = 'US$ (em bilh\u00f5es)')) %>%
        add_trace(hoverinfo = 'y')
    })
}
  

shinyApp(ui, server)
 

