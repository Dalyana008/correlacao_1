# pacotes
pacman::p_load(
  caret, ggplot2, plotly, rattle
)


Controle_corrupcao_eecutivo <- Dados_controle_executivo %>% dplyr::filter(Region != 'Latin America  & Caribbean')

set.seed(3)

# Pré-processamento
particaoCORRUPCAO_EXECUTIVO = createDataPartition(1:nrow(Controle_corrupcao_eecutivo), p=0.7) # cria a partição 70-30
treinoCORRUPCAO_EXECUTIVO = Controle_corrupcao_eecutivo[particaoCORRUPCAO_EXECUTIVO$Resample1, ] # treino
testeCORRUPCAO_EXECUTIVO = Controle_corrupcao_eecutivo[-particaoCORRUPCAO_EXECUTIVO$Resample1, ] # - treino = teste

# Controle de treinamento
train.control <- trainControl(method = "cv", number = 100, verboseIter = T) # controle de treino

# Mineração e predição com Árvores de Decisão
## Árvore de Decisão

CONTROLE_CORRUPCAO_RPART <- train(
  ausencia_corrupcao ~ transparencia_governamental + acesso_informacao + controle_do_crime  + fiscalizacao_regulatoria,
  data = treinoCORRUPCAO_EXECUTIVO, 
  method = "rpart", 
  trControl = train.control,
  tuneGrid = expand.grid(cp = c(0.00362, runif(19, 0, 0.25))), tuneLength = 20
)



plot(CONTROLE_CORRUPCAO_RPART)

fancyRpartPlot(CONTROLE_CORRUPCAO_RPART$finalModel) # desenho da árvore

plot(varImp(CONTROLE_CORRUPCAO_RPART)) # importância das variáveis

predicaoTree = predict(CONTROLE_CORRUPCAO_RPART, newdata = testeCORRUPCAO_EXECUTIVO)

postResample(testeCORRUPCAO_EXECUTIVO[ , 7], predicaoTree) # teste de performance da Árvore Condicional

base_avaliacao <- data.frame(
  Observado = testeCORRUPCAO_EXECUTIVO[ , 7],
  Predição = predicaoTree)

predicao_arvore <- base_avaliacao %>% 
  ggplot(aes(x=Observado, y=Predição)) + 
  geom_point() + # cria os pontos
  geom_smooth() # cria a curva de associação
ggplotly(predicao_arvore)
