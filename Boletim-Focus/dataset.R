###################
### BIBLIOTECAS ###
###################

library(dplyr)


##########################
### BACEN API FUNCTION ###
##########################

#' Get annual market expectations of economic indicators
#'
#' Statistics for the annual expectations of economic indicators: mean, median, standard
#' deviate, minimum, maximum and the coefficient of variation.
#' All statistics are computed based on annual expectations provided by many financial
#' institutions in Brazil: banks, funds, risk managers, so on and so forth.
#' These expections and its statistics are used to build the FOCUS Report weekly
#' released by the Brazilian Central Bank.
#'
#' There are annual expectations available for the following indicators: Balança Comercial,
#' Balanço de Pagamentos, Fiscal, IGP-DI, IGP-M, INPC, IPA-DI, IPA-M, IPCA, IPCA-15, IPC-Fipe,
#' Preços administrados por contrato e monitorados, Produção industrial, PIB Agropecuária,
#' PIB Industrial, PIB Serviços, PIB Total, Meta para taxa over-selic, Taxa de câmbio.
#'
#' @param indic a character vector with economic indicators names: Balança Comercial,
#' Balanço de Pagamentos, Fiscal, IGP-DI, IGP-M, INPC, IPA-DI, IPA-M, IPCA, IPCA-15, IPC-Fipe,
#' Preços administrados por contrato e monitorados, Produção industrial, PIB Agropecuária,
#' PIB Industrial, PIB Serviços, PIB Total, Meta para taxa over-selic, Taxa de câmbio.
#' They are case sensitive and don't forget the accents.
#' @param start_date series initial date. Accepts ISO character formated date and \code{Date}.
#' @param end_date series final date. Accepts ISO character formated date and \code{Date}.
#' @param ... additional parameters to be passed to the API
#'
#' \code{indic} argumento must be one of these: Balança Comercial,
#' Balanço de Pagamentos, Fiscal, IGP-DI, IGP-M, INPC, IPA-DI, IPA-M, IPCA, IPCA-15, IPC-Fipe,
#' Preços administrados por contrato e monitorados, Produção industrial, PIB Agropecuária,
#' PIB Industrial, PIB Serviços, PIB Total, Meta para taxa over-selic, Taxa de câmbio.
#' Respecting the case, blank spaces and accents.
#'
#' The \code{...} is to be used with API's parameters. \code{$top} to specify
#' the maximum number of rows to be returned, this returns the \code{$top} rows,
#' in chronological order. There is also \code{$skip} to ignore the first rows.
#'
#' @return
#' A \code{data.frame} with the following ten columns: \code{date}, \code{indic},
#' \code{indic_detail}, \code{reference_year}, \code{mean}, \code{median}, \code{sd},
#' \code{coefvar}, \code{min}, \code{max}.
#'
#' @examples
#' indic <- c("PIB Total", "Fiscal")
#' end_date <- "2018-01-31"
#' x <- get_annual_market_expectations(indic, end_date = end_date, `$top` = 10)
#'
#' @export

get_annual_market_expectations <- function(indic, start_date = NULL,
                                           end_date = NULL, ...) {
  valid_indic <- c("Balan\u00e7a Comercial",
                   "Balan\u00e7o de Pagamentos",
                   "Fiscal",
                   "IGP-DI",
                   "IGP-M",
                   "INPC",
                   "IPA-DI",
                   "IPA-M",
                   "IPCA",
                   "IPCA-15",
                   "IPC-Fipe",
                   "Pre\u00e7os administrados por contrato e monitorados",
                   "Produ\u00e7\u00e3o industrial",
                   "PIB Agropecu\u00e1ria",
                   "PIB Industrial",
                   "PIB Servi\u00e7os",
                   "PIB Total",
                   "Meta para taxa over-selic",
                   "Taxa de c\u00e2mbio")
  
  check_indic <- indic %in% valid_indic
  if (!all(check_indic))
    stop("Invalid indic argument: ",
         paste(indic[!check_indic], collapse = ", "))
  
  url <- annual_market_expectations_url(indic, start_date, end_date, ...)
  
  data_ <- jsonlite::fromJSON(url)
  
  df_ <- tibble::as_tibble(data_$value)
  names(df_) <- c("indic", "indic_detail", "date", "reference_year", "mean",
                  "median", "sd", "coefvar", "min", "max")
  df_$date <- as.Date(df_$date)
  df_
}

annual_market_expectations_url <- function(indic, start_date, end_date, ...) {
  indic_filter <- paste(sprintf("Indicador eq '%s'", indic), collapse = " or ")
  indic_filter <- paste0("(", indic_filter, ")")
  
  sd_filter <- if (!is.null(start_date))
    sprintf("Data ge '%s'", start_date) else NULL
  
  ed_filter <- if (!is.null(end_date))
    sprintf("Data le '%s'", end_date) else NULL
  
  filter__ <- paste(c(indic_filter, sd_filter, ed_filter), collapse = " and ")
  
  httr::modify_url(
    "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais",
    query = list(
      `$filter` = filter__,
      `$format` = "application/json",
      `$orderby` = "Data desc",
      `$select` = "Indicador,IndicadorDetalhe,Data,DataReferencia,Media,Mediana,DesvioPadrao,CoeficienteVariacao,Minimo,Maximo",
      ...)
  )
}


###################
### PIB - FOCUS ###
###################

indic_pib <- c('Produ\u00e7\u00e3o industrial', 'PIB Agropecu\u00e1ria', 'PIB Industrial', 'PIB Servi\u00e7os', 'PIB Total')
data_inicio <- '2018-01-01'

pib <- data.frame()

for (i in 1:length(indic_pib)){
  temp <- get_annual_market_expectations(indic_pib[i],
                                         start_date = data_inicio)
  pib <- bind_rows(pib,temp)
  
}

rm(temp,i,indic_pib,data_inicio)


################
### INFLAÇÃO ###
################

indic_inflacao <- c('IGP-DI','IGP-M','INPC','IPA-DI', 'IPA-M','IPCA', 'IPCA-15','Pre\u00e7os administrados por contrato e monitorados')
data_inicio <- '2018-01-01'

inflacao <- data.frame()

for (i in 1:length(indic_inflacao)){
  temp <- get_annual_market_expectations(indic_inflacao[i],
                                         start_date = data_inicio)
  inflacao <- bind_rows(inflacao,temp)
  
}

rm(temp,i,indic_inflacao,data_inicio)


######################
### CÂMBIO - FOCUS ###
######################

indic_cambio <- 'Taxa de c\u00e2mbio'
data_inicio <- '2018-01-01'

cambio <- get_annual_market_expectations(indic_cambio, start_date = data_inicio)

rm(indic_cambio, data_inicio)


#####################
### SELIC - FOCUS ###
#####################

indic_selic <- 'Meta para taxa over-selic'
data_inicio <- '2018-01-01'

selic <- get_annual_market_expectations(indic_selic, start_date = data_inicio)

rm(indic_selic, data_inicio)


#########################
### BALANÇA COMERCIAL ###
#########################

indic_balanca_comercial <- 'Balan\u00e7a Comercial'
data_inicio <- '2018-01-01'

balanca_comercial <- get_annual_market_expectations(indic_balanca_comercial, start_date = data_inicio)

rm(indic_balanca_comercial, data_inicio)


##########################
### BALANÇO PAGAMENTOS ###
##########################

indic_balanco_pagamentos <- 'Balan\u00e7o de Pagamentos'
data_inicio <- '2018-01-01'

balanco_pagamentos <- get_annual_market_expectations(indic_balanco_pagamentos, start_date = data_inicio)

rm(indic_balanco_pagamentos, data_inicio)


##################
### SIT FISCAL ###
##################

indic_fiscal <- 'Fiscal'
data_inicio <- '2018-01-01'

fiscal <- get_annual_market_expectations(indic_fiscal, start_date = data_inicio)

rm(indic_fiscal, data_inicio)

