# Matriz de Confusão: matriz que relaciona as classes observadas (também chamadas de referência) e as classes preditas. Para melhor interpretação, oferece várias estatísticas auxiliares. Vamos ver as principais delas
# Accuracy (Acc) = Acuidade, ou performance geral do modelo - total de acertos, sem considerar nenhuma penalidade ou ajuste
# No Information Rate (NIR) = proporção da classe mais frequente - indica o quão a classe mais frequente está presente nos dados. É um valor de referência para compararmos com a acuidade, uma vez que o modelo poderia ganhar performance artificialmente aprendendo a sempre "chutar" na classe mais frequente. É oferecido também um teste de hipótese para verificar a hipótese de que a Acc é maior que o NIR. 
# Kappa = coeficiente kappa de Cohen - em geral, mede a concordância de duas classificações. No caso de ML, tem a ver com a estimativa de acuidade controlada pela possibilidade de classificação aleatória. Assim, permite saber se o modelo é bom, mesmo considerando a chance de "sortear" o resultado. 

#Classificação
#Aprendizagem de máquina
#02,11,23


# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)
               
# Carregando banco de dados

library(readxl)
controle_corrupcao <- read_excel("C:/Users/daly_/OneDrive/Estatísticas doutorado/controle_corrupcao.xlsx")
View(controle_corrupcao)
               
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

predicaocontrole_RF_CLASS = predict(controle_RF_CLASS, testeControle) # criar predição
cm_RF_ControleCLASS <- confusionMatrix(predicaocontrole_RF_CLASS, testeControle$ausencia_corrupcao)
cm_RF_ControleCLASS
cm_RF_ControleCLASS$table

# Expected Accuracy (AccE) = Acuidade Esperada = estimativa de acuidade "esperada", ou seja, uma acuidade mínima que poderia ser conseguida simplesmente "chutando" a classe de forma aleatória. 

gtBaixa <- cm_RF_ControleCLASS$table[1]+cm_RF_ControleCLASS$table[2]
gtAlta <- cm_RF_ControleCLASS$table[3]+cm_RF_ControleCLASS$table[4]

pdBaixa <- cm_RF_ControleCLASS$table[1]+cm_RF_ControleCLASS$table[3]
pdAlta <- cm_RF_ControleCLASS$table[2]+cm_RF_ControleCLASS$table[4]

gtTotal <- gtAlta + gtBaixa

estAcc <- (gtBaixa*pdBaixa/gtTotal^2)+(gtAlta*pdAlta/gtTotal^2)
estAcc