

#Limpar o ambiente
rm(list = ls())

#Instalar e/ou carregar os pacotes
pacotes = c("dplyr", "tidylog")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))



#Pegando os dados principais 

setwd("source")

ds_salaries <- read.csv("data/ds_salaries.csv", stringsAsFactors=TRUE)


#Pegando as informações dos nomes dos países
country_code <-  read.csv("data/country_code.csv", stringsAsFactors=TRUE)



#Pegando as informações das internets dos paises
internet <-  read.csv("data/internet broadband and mobile speeds by country.csv", stringsAsFactors=TRUE)


#Juntando as bases country_code e internet

internet_code<- internet%>%dplyr::left_join(country_code, by = c('Country' = 'Name'))



#Juntando as bases internet_code e ds_salaries

salaries_internet<-ds_salaries%>%dplyr::left_join(internet_code, by = c('employee_residence' = 'Code'))


#Criando a variável target 100% remoto ou não agrupar por país

salaries_internet<- salaries_internet%>%dplyr::mutate(target = dplyr::case_when(remote_ratio == 100 ~ 'SIM',
                                                                                TRUE ~ 'NAO'),
                                                      is_us = dplyr::case_when(Country == 'United States' ~ 'USA',
                                                                               TRUE ~ 'OUTRO'))

#Salvando o objeto
save(salaries_internet, file = "data/data.Rdata")

