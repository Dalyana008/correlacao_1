# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados
library(readxl)
controle_executivo_corrupcao <- read_excel("C:/Users/daly_/OneDrive/Estatísticas doutorado/controle_executivo_corrupcao.xlsx")
View(controle_executivo_corrupcao) # carregando a base já tratada para o ambiente do R

# Dummies
controle_D <- acm.disjonctif(as.data.frame(controle_executivo_corrupcao$`Income Group*`))
names(controle_D) <- c('Low', 'Higth', 'Upper_middle', 'Lower_moddle')

controle_executivo_corrupcao<- cbind(controle_executivo_corrupcao, controle_D)

# Discretização
controle_executivo_corrupcao$ausencia_corrupcaoDisc <- discretize(controle_executivo_corrupcao$ausencia_corrupcao, method = "interval", breaks = 2, labels = c("baixa", "alta"))

table(controle_executivo_corrupcao$ausencia_corrupcaoDisc)

# Treino e Teste: Pré-processamento
particaocorrupcao = createDataPartition(controle_executivo_corrupcao$ausencia_corrupcao, p=.7, list = F) # cria a partição 70-30
treinoCorrup = controle_executivo_corrupcao[particaocorrupcao, ] # treino
testeCorrup = controle_executivo_corrupcao[-particaocorrupcao, ] # - treino = teste

table(treinoCorrup$ausencia_corrupcaoDisc)

# down / under
treinocorrupDs <- downSample(x = treinoCorrup[, -ncol(treinoCorrup)], y = treinoCorrup$ausencia_corrupcaoDisc)
table(treinocorrupDs$Class)   

# up
treinocontroleUs <- upSample(x = treinoCorrup[, -ncol(treinoCorrup)], y = treinoCorrup$ausencia_corrupcaoDisc)
table(treinocontroleUs$Class)  
