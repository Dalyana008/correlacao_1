# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, DMwR, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados
# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, DMwR, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados
library(readxl)
> controle_corrupcao <- read_excel("C:/Users/daly_/OneDrive/Estatísticas doutorado/controle_corrupcao.xlsx") # carregando a base já tratada para o ambiente do R

# Dummies
controle_D <- acm.disjonctif(as.data.frame(controle_corrupcao$`Income Group*`))
names(controle_D) <- c('Low', 'High', 'Upper_middle', 'Lower_middle')

controle_corrupcao <- cbind(controle_corrupcao, controle_D)

# Discretização
controle_D$auseenciaDisc <- discretize(controle_corrupcao$ausencia_corrupcao, method = "interval", breaks = 2, labels = c("baixa", "alta"))

table(controle_corrupcao$auseenciaDisc)

# Treino e Teste: Pré-processamento
particaoControle = createDataPartition(controle_corrupcao$ausencia_corrupcao, p=.7, list = F) # cria a partição 70-30
treinoControle = controle_corrupcao[particaoControle, ] # treino
testeControle = controle_corrupcao[-particaoControle, ] # - treino = teste

prop.table(table(treinoControle$auseenciaDisc))

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

matrizCusto <- matrix(c(0,1,1000,0), ncol = 2)
rownames(matrizCusto) <- levels(treinoControle$auseenciaDisc)
colnames(matrizCusto) <- levels(treinoControle$auseenciaDisc)
matrizCusto

Controle_RF_CLASS <- randomForest(auseenciaDisc ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo  + controle_judiciario + controle_do_crime, data = treinoControle, method = "cforest", parms = list(loss = matrizCusto))
Controle_RF_CLASS

CONTROLE_C5_CLASS <- train(auseenciaDisc ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo  + controle_judiciario + controle_do_crime, data = treinoControle, method = "C5.0Cost", trControl = train.control)
 CONTROLE_C5_CLASS

predicaoControle_RF_CLASS = predict(Controle_RF_CLASS, testeControle) # criar predição
cmControle_RF_CLASS <- confusionMatrix(predicaoControle_RF_CLASS, testeControle$auseenciaDisc)
cmControle_RF_CLASS

predicaoControle_C5_CLASS = predict(CONTROLE_C5_CLASS, testeControle) # criar predição
cmControle_C5_CLASS <- confusionMatrix(predicaoControle_C5_CLASS,testeControle$auseenciaDisc)
cmControle_C5_CLASS
