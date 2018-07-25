###################
### BIBLIOTECAS ###
###################

library(rbcb)
library(dplyr)

####################
### PROXY CONFIG ###
####################

# Proxy do Sebrae Nacional
# set_config(use_proxy(url='10.1.140.76',port=8080))

###################
### PIB - FOCUS ###
###################

indic_pib <- c('Produção industrial', 'PIB Agropecuária', 'PIB Industrial', 'PIB Serviços', 'PIB Total')
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

indic_inflacao <- c('IGP-DI','IGP-M','INPC','IPA-DI', 'IPA-M','IPCA', 'IPCA-15','Preços administrados por contrato e monitorados')
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

indic_cambio <- 'Taxa de câmbio'
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

indic_balanca_comercial <- 'Balança Comercial'
data_inicio <- '2018-01-01'

balanca_comercial <- get_annual_market_expectations(indic_balanca_comercial, start_date = data_inicio)

rm(indic_balanca_comercial, data_inicio)


##########################
### BALANÇO PAGAMENTOS ###
##########################

indic_balanco_pagamentos <- 'Balanço de Pagamentos'
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
