###################
### BIBLIOTECAS ###
###################

library(shiny)

####################
### SHINY DEPLOY ###
####################

ui <- shinyUI(pageWithSidebar(  
  headerPanel("Boletim Focus - Relatório de Mercado - Expectativas",windowTitle = 'Boletim-Focus'),
  sidebarPanel(
    conditionalPanel(condition = "input.indicador == 'PIB'",
                     dateRangeInput(inputId = 'data_pib',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput(inputId = 'num_pib', 
                                  label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), 
                                  min = year(Sys.Date()), 
                                  max = year(Sys.Date())+4),
                     selectInput('indic_pib', label = h3('Indicador:'),
                                 choices = list('Produção industrial' = 'Produção industrial', 
                                                'PIB Agropecuária' = 'PIB Agropecuária', 
                                                'PIB Industrial' = 'PIB Industrial', 
                                                'PIB Serviços' = 'PIB Serviços', 
                                                'PIB Total' = 'PIB Total'),
                                 selected = 'PIB Total'),
                     selectInput('metrica_pib', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    ),
    conditionalPanel(condition = "input.indicador == 'INFLAÇÃO'",
                     dateRangeInput(inputId = 'data_inflacao',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput('num_inflacao', label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), min = year(Sys.Date()), max = year(Sys.Date())+4),
                     selectInput('indic_inflacao', label = h3('Indicador:'),
                                 choices = list('IGP-DI' = 'IGP-DI', 
                                                'IGP-M' = 'IGP-M', 
                                                'INPC' = 'INPC',
                                                'IPA-DI' = 'IPA-DI',
                                                'IPA-M' = 'IPA-M', 
                                                'IPCA' = 'IPCA',
                                                'IPCA-15' = 'IPCA-15',
                                                'Preços administrados por contrato e monitorados' = 'Preços administrados por contrato e monitorados'),
                                 selected = 'IPCA'),
                     selectInput('metrica_inflacao', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    ),
    conditionalPanel(condition = "input.indicador == 'CÂMBIO'",
                     dateRangeInput(inputId = 'data_cambio',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput('num_cambio', label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), min = year(Sys.Date()), max = year(Sys.Date())+4),
                     selectInput('indic_cambio', label = h3('Indicador:'),
                                 choices = list('Taxa de câmbio' = 'Taxa de câmbio'),
                                 selected = 'Taxa de câmbio'),
                     selectInput('metrica_cambio', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    ),
    conditionalPanel(condition = "input.indicador == 'SELIC'",
                     dateRangeInput(inputId = 'data_selic',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput('num_selic', label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), min = year(Sys.Date()), max = year(Sys.Date())+4),
                     selectInput('indic_selic', label = h3('Indicador:'),
                                 choices = list('Meta para taxa over-selic' = 'Meta para taxa over-selic'),
                                 selected = 'Meta para taxa over-selic'),
                     selectInput('metrica_selic', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    ),
    conditionalPanel(condition = "input.indicador == 'BALANÇA COMERCIAL'",
                     dateRangeInput(inputId = 'data_balanca_comercial',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput('num_balanca_comercial', label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), min = year(Sys.Date()), max = year(Sys.Date())+4),
                     selectInput('indic_balanca_comercial', label = h3('Indicador:'),
                                 choices = list('Balança Comercial' = 'Balança Comercial'),
                                 selected = 'Balança Comercial'),
                     selectInput('metrica_balanca_comercial', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    ),
    conditionalPanel(condition = "input.indicador == 'BALANÇO DE PAGAMENTOS'",
                     dateRangeInput(inputId = 'data_balanco_pagamentos',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput('num_balanco_pagamentos', label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), min = year(Sys.Date()), max = year(Sys.Date())+4),
                     selectInput('indic_balanco_pagamentos', label = h3('Indicador:'),
                                 choices = list('Balanço de Pagamentos' = 'Balanço de Pagamentos'),
                                 selected = 'Balanço de Pagamentos'),
                     selectInput('metrica_balanco_pagamentos', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    ),
    conditionalPanel(condition = "input.indicador == 'FISCAL'",
                     dateRangeInput(inputId = 'data_fiscal',
                                    label = h3('Selecione o período dos Boletins Focus:'),
                                    format = 'mm/yyyy',
                                    language='pt-BR',
                                    min = '2018-01-01',
                                    max = Sys.Date(),
                                    start = '2018-01-01',
                                    end = Sys.Date(),
                                    startview = 'year',
                                    separator = '-'),
                     numericInput('num_fiscal', label = h3('Expectativa do Mercado para o Ano:'), 
                                  value = year(Sys.Date()), min = year(Sys.Date()), max = year(Sys.Date())+4),
                     selectInput('indic_fiscal', label = h3('Indicador:'),
                                 choices = list('Fiscal' = 'Fiscal'),
                                 selected = 'Fiscal'),
                     selectInput('metrica_fiscal', label = h3('Métrica:'), 
                                 choices = list('Média' = 'mean', 'Mediana' = 'median', 'Mínimo' = 'min', 'Máximo' = 'max', 'Desvio Padrão' = 'sd', 'Coeficiente de Variação' = 'coefvar'),
                                 selected = 'mean')
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('PIB',
               plotOutput('Graf_PIB1'),
               plotOutput('Graf_PIB2')),
      tabPanel('INFLAÇÃO',
               plotOutput('Graf_Inflacao1')),
      tabPanel('CÂMBIO',
               plotOutput('Graf_Cambio1')),
      tabPanel('SELIC',
               plotOutput('Graf_Selic1')),
      tabPanel('BALANÇA COMERCIAL', paste('Em construção')),
      # plotOutput('Graf_Balanca_Comercial1')),
      tabPanel('BALANÇO DE PAGAMENTOS', paste('Em construção')),
      # plotOutput('Graf_Balanco_Pagamentos1')),
      tabPanel('FISCAL', paste('Em construção')),
      # plotOutput('Graf_Fiscal1')),
      id = "indicador")
  )
)
)





server <- function(input, output, session){
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(plotly)
  
  dates_pib <- reactiveValues()
  observe({
    dates_pib$SelectedDates <- c(as.character(format(input$data_pib[1],format = '%m/%Y')),
                                 as.character(format(input$data_pib[2],format = '%m/%Y')))
  })
  
  base_pib <- reactive({
    pib %>%
      select(indic,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_pib,
             # reference_year==input$num_pib,
             metric == input$metrica_pib,
             date >= input$data_pib[1] & date <= input$data_pib[2])
  })
  
  output$Graf_PIB1 <- renderPlot({
    base_pib() %>%
      filter(date == max(date)) %>%
      ggplot(aes(x = factor(reference_year), y=value)) +
      geom_bar(stat = 'identity', fill = 'darkblue') +
      labs(title=paste0('Crescimento Esperado do PIB'),
           caption='Fonte: Boletim Focus - Banco Central') +
      xlab('Ano') +
      ylab('% a.a.') +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0),
            panel.background = element_rect(fill = 'white', colour = 'grey50'))
  })
    
  output$Graf_PIB2 <- renderPlot({
    base_pib() %>%
      filter(reference_year==input$num_pib) %>%
      ggplot(aes(date, value)) +
      geom_line(size=1.2, colour='darkblue') +
      labs(title=paste0('Crescimento Esperado para ',input$num_pib),
           caption='Fonte: Boletim Focus - Banco Central') +
      xlab('') +
      ylab('% a.a.') +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0),
            panel.background = element_rect(fill = 'white', colour = 'grey50'))
  })
  
  
  dates_inflacao <- reactiveValues()
  observe({
    dates_inflacao$SelectedDates <- c(as.character(format(input$data_inflacao[1],format = '%m/%Y')),
                                      as.character(format(input$data_inflacao[2],format = '%m/%Y')))
  })
  
  base_inflacao <- reactive({
    inflacao %>%
      select(indic,date,reference_year, mean, median, min, max, sd, coefvar) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_inflacao,
             reference_year==input$num_inflacao,
             metric == input$metrica_inflacao,
             date >= input$data_inflacao[1] & date <= input$data_inflacao[2])
  })
  
  output$Graf_Inflacao1 <- renderPlot({
    ggplot(base_inflacao(), aes(date, value)) +
      geom_line(size=1.2, colour='darkblue') +
      labs(title=paste0('Expectativa de Inflação para ',input$num_inflacao),
           caption='Fonte: Boletim Focus - Banco Central') +
      xlab('') +
      ylab('% a.a.') +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0),
            panel.background = element_rect(fill = 'white', colour = 'grey50'))
  })
  
  dates_cambio <- reactiveValues()
  observe({
    dates_cambio$SelectedDates <- c(as.character(format(input$data_cambio[1],format = '%m/%Y')),
                                    as.character(format(input$data_cambio[2],format = '%m/%Y')))
  })
  
  base_cambio <- reactive({
    cambio %>%
      filter(indic_detail == 'Fim do ano') %>%
      select(indic,date,reference_year, mean, median, min, max) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_cambio,
             reference_year==input$num_cambio,
             metric == input$metrica_cambio,
             date >= input$data_cambio[1] & date <= input$data_cambio[2])
  })
  
  output$Graf_Cambio1 <- renderPlot({
    ggplot(base_cambio(), aes(date, value)) +
      geom_line(size=1.2, colour='darkblue') +
      labs(title=paste0('Expectativa da Taxa de Câmbio para o Fim do Ano de ',input$num_cambio),
           caption='Fonte: Boletim Focus - Banco Central') +
      xlab('') +
      ylab('% a.a.') +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0),
            panel.background = element_rect(fill = 'white', colour = 'grey50'))
  })
  
  base_selic <- reactive({
    selic %>%
      filter(indic_detail == 'Fim do ano') %>%
      select(indic,date,reference_year, mean, median, min, max) %>%
      gather("metric", "value", -c(indic,date,reference_year)) %>%
      filter(indic==input$indic_selic,
             reference_year==input$num_selic,
             metric == input$metrica_selic,
             date >= input$data_selic[1] & date <= input$data_selic[2])
  })
  
  output$Graf_Selic1 <- renderPlot({
    ggplot(base_selic(), aes(date, value)) +
      geom_line(size=1.2, colour='darkblue') +
      labs(title=paste0('Expectativa da Selic para o Fim do Ano de ',input$num_selic),
           caption='Fonte: Boletim Focus - Banco Central') +
      xlab('') +
      ylab('% a.a.') +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0),
            panel.background = element_rect(fill = 'white', colour = 'grey50'))
  })
  
  
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
