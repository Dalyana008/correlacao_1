#Classificação
#Aprendizagem de máquina
#02,11,23


# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# Dummies
controle_D <- acm.disjonctif(as.data.frame(controle_corrupcao$`Income Group*`))
names(controle_D) <- c('Low', 'High', 'Lower_middle', 'Upper_middle')

controle_corrupcao <- cbind(controle_corrupcao, controle_D)




# Discretização
controle_corrupcao$ausencia_corrupcao <- discretize(controle_corrupcao$ausencia_corrupcao, method = "frequency", breaks = 2, labels = c("baixa", "alta"))

# Treino e Teste: Pré-processamento
particaoControle = createDataPartition(controle_corrupcao$ausencia_corrupcao, p=.7, list = F) # cria a partição 70-30
treinoControle = controle_corrupcao[particaoControle, ] # treino
testeControle = controle_corrupcao[-particaoControle, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

# Treinamentos
## Máquina de Vetor se Suporte (SVM)
controle_SVM_CLASS <- train(ausencia_corrupcao ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo + controle_judiciario + transparencia_governamental, data = treinoControle, method = "svmLinear", trControl = train.control)


controle_SVM_CLASS # sumário da máquina de vetor de suporte
plot(varImp(controle_SVM_CLASS))

# criar a máquina de vetor de suporte
svmCONTROLECLass = svm(ausencia_corrupcao ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo + controle_judiciario + transparencia_governamental, data = treinoControle, cost = 10, scale = F)
svmCONTROLECLass
plot(svmCONTROLECLass, treinoControle, auditoria_independente ~ transparencia_governamental)

## Árvore de Decisão
controle_RPART_CLASS <- train(ausencia_corrupcao ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo + controle_judiciario + transparencia_governamental, data = treinoControle, method = "rpart", trControl = train.control)

summary(controle_RPART_CLASS)
fancyRpartPlot(controle_RPART_CLASS$finalModel) # desenho da árvore
plot(varImp(controle_RPART_CLASS)) # importância das variáveis

# Bagging com Floresta Aleatória
controle_RF_CLASS <- train(ausencia_corrupcao ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo + controle_judiciario + transparencia_governamental, data = treinoControle, method = "cforest", trControl = train.control)

plot(controle_RF_CLASS) # evolução do modelo
plot(varImp(controle_RF_CLASS)) # plot de importância

# Boosting com Boosted Generalized Linear Model
controle_ADA_CLASS <- train(ausencia_corrupcao ~ Low + High + Lower_middle + Upper_middle + auditoria_independente + controle_legislativo + controle_judiciario + transparencia_governamental,, data = treinoControle, method = "glmboost", trControl = train.control)

plot(controle_ADA_CLASS) # evolução do modelo
print(controle_ADA_CLASS) # modelo
summary(controle_ADA_CLASS) # sumário

melhor_modelo <- resamples(list(SVM = controle_SVM_CLASS, RPART = controle_RPART_CLASS, RF = controle_RF_CLASS, ADABOOST = controle_ADA_CLASS))
melhor_modelo

summary(melhor_modelo)
colnames(testeControle)

predVals <- extractPrediction(list(SVM = controle_SVM_CLASS, RPART = controle_RPART_CLASS, RF = controle_RF_CLASS, ADABOOST = controle_ADA_CLASS), testX = testeControle[, c(8,9,10,20, 59:62)], testY = testeControle$ausencia_corrupcao) 

plotObsVsPred(predVals)
