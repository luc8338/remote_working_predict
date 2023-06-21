
#Limpar o ambiente
rm(list = ls())

#Instalar e/ou carregar os pacotes
pacotes = c("dplyr", "tidylog", "ggplot2","ggeasy")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

#carregar os dados
load("data/data.Rdata")


#contando o número de NA
countNA = function(x) sum(is.na(x)) 

rows_with_na = salaries_internet %>%
  dplyr::mutate(flag_na = as.factor(dplyr::case_when(apply(salaries_internet, 1, countNA) == 0 ~ 'Sem', TRUE ~ 'Com'))) %>%
  dplyr::group_by(flag_na) %>%
  dplyr::summarise(na = dplyr::n(), prop = round(na/nrow(salaries_internet) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(na, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

plot_na = ggplot(data=rows_with_na, aes(x = flag_na, y = na)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Nulos") +
  ylab("Quantidade") +
  ggtitle(label = "Linhas com Valores Nulos x sem Valores Nulos") +
  ggeasy::easy_center_title()

#salvando o plot com NA
ggsave(filename = "plots/rows_with_na.png", plot = plot_na, height = 5, width = 5.5)


#Balanceamento da população
remoto = salaries_internet %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(qt = dplyr::n(),
                   prop = round(qt/nrow(salaries_internet) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(qt, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

plot_balance = ggplot(data=remoto, aes(x = target, y = qt)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Remoto") +
  ylab("Quantidade") +
  ggtitle(label = "Quantidade 100% remoto") +
  ggeasy::easy_center_title()

#salvando o plot
ggsave(filename = "plots/remoto_balance.png", plot = plot_balance, height = 5, width = 5.5)

#distribuição a senioridade por 100% remoto

senior <- salaries_internet %>% dplyr::mutate(experience_level = dplyr::case_when(experience_level == 'SE' ~ 'Senior',
                                                                                  experience_level == 'EN' ~ 'Entry level',
                                                                                  experience_level == 'EX' ~ 'Executive level',
                                                                                  experience_level == 'MI' ~ 'Mid/Intermediate level',))

plot_senior<-ggplot(senior) +
  aes(x = experience_level) +
  geom_bar(fill = "steelblue") + 
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Nível de experiência") +
  ylab("Quantidade") +
  ggtitle(label = "Senioridade por 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/senior.png", plot = plot_senior, height = 5, width = 5.5)

#distribuição do tipo de cargo por 100% remoto

cargo <- salaries_internet %>% dplyr::mutate(employment_type = dplyr::case_when(employment_type == 'CT' ~ 'Contractor ',
                                                                                employment_type == 'FL' ~ 'Freelancer',
                                                                                employment_type == 'FT' ~ 'Full-time',
                                                                                employment_type == 'PT' ~ 'Part-time'))

plot_cargo<-ggplot(cargo) +
  aes(x = employment_type) +
  geom_bar(fill = "steelblue") + 
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Tipo de cargo") +
  ylab("Quantidade") +
  ggtitle(label = "Tipo de cargo por 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/cargo.png", plot = plot_cargo, height = 5, width = 5.5)


#distribuição do tamanho da empresa por 100% remoto

tamanho <- salaries_internet %>% dplyr::mutate(company_size = dplyr::case_when(company_size == 'L' ~ 'Large',
                                                                               company_size == 'M' ~ 'Medium',
                                                                               company_size == 'S' ~ 'Small'))

plot_tamanho<-ggplot(tamanho) +
  aes(x = company_size) +
  geom_bar(fill = "steelblue") + 
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Tamanho da empresa") +
  ylab("Quantidade") +
  ggtitle(label = "Tamanho por 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/tamanho.png", plot = plot_tamanho, height = 5, width = 5.5)


#distribuição do salario por 100% remoto

options(scipen = 999999)

plot_salario<-ggplot(salaries_internet) +
  aes(x = salary_in_usd) +
  geom_density(adjust = 1L, fill = "steelblue") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Salário em dolar") +
  ylab("Densidade") +
  ggtitle(label = "Salário em dolar por 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/salario.png", plot = plot_salario, height = 5, width = 5.5)


#distribuição de quem mora nos EUA ou não por 100% remoto


plot_in_us<-ggplot(salaries_internet) +
  aes(x = is_us) +
  geom_bar(fill = "steelblue") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("País") +
  ylab("Quantidade") +
  ggtitle(label = "Mora nos EUA x 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/in_us.png", plot = plot_in_us, height = 5, width = 5.5)


#distribuição da velocidade da internet por 100% remoto


plot_velocidade<-ggplot(salaries_internet) +
  aes(x = Broadband.Mbps) +
  geom_density(adjust = 1L, fill = "steelblue") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Velocidade da internet") +
  ylab("Densidade") +
  ggtitle(label = "Velocidade da internet por 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/velocidade.png", plot = plot_velocidade, height = 5, width = 5.5)




#distribuição da velocidade da internet mobile por 100% remoto


plot_velocidade_movel<-ggplot(salaries_internet) +
  aes(x = Mobile.Mbps) +
  geom_density(adjust = 1L, fill = "steelblue") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Velocidade da internet") +
  ylab("Densidade") +
  ggtitle(label = "Velocidade da internet movel por 100% remoto") +
  ggeasy::easy_center_title() +
  facet_wrap(vars(target))


#salvando o plot
ggsave(filename = "plots/velocidade_movel.png", plot = plot_velocidade_movel, height = 5, width = 5.5)
