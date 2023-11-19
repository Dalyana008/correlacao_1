pacman::p_load(
  caret, corrplot, data.table, dplyr, fastDummies, ggplot2, SmartEDA, glmnet,tidyverse
)

# Preparação
particaoMtcars = createDataPartition(mtcars$mpg, p = 0.7, list = FALSE) 
treinoMtcars = mtcars[particaoMtcars, ]
testeMtcars = mtcars[-particaoMtcars, ]

mtcars_formula <- mpg ~ . 

# Modelagem
lista_modelos <- c('lm', 'glmnet', 'glmboost', 'rpart', 'cforest')
total_cv <- 10
train.control <- trainControl(method = "cv", number = total_cv, verboseIter = TRUE)

pacman::p_load(caretEnsemble, doParallel)
registerDoParallel(cores = detectCores() - 1)

mtcars_modelos <- caretList(
  mtcars_formula, 
  data = treinoMtcars, 
  methodList = lista_modelos, 
  metric = "RMSE",
  trControl = train.control,
  tuneLength = 5
)

# Análise de Importância de Variáveis
importancia_variaveis <- varImp(mtcars_modelos$cforest)
print(importancia_variaveis)

# Gráfico de Importância de Variáveis
grafico_varImp <- ggplot(data = importancia_variaveis, aes(x = reorder(var, -Overall), y = Overall)) + 
  geom_bar(stat = "identity", fill = '#007095') + 
  theme_minimal() + 
  coord_flip() + 
  labs(
    title = ~ underline("Importância das variáveis usadas no modelo"), 
    subtitle = "mtcars",
    caption = 'Modelo: Floresta Aleatória',
    x = '',
    y = 'Importância Relativa'
  ) + 
  theme(
    plot.title = element_text(face = 'bold', lineheight = 1, size = 16, color = "#007095"),
    plot.subtitle = element_text(face = 'italic', size = 12, color = "#007095"),
    plot.caption = element_text(size = 10, color = "#007095"),
    strip.text = element_text(size = 10, color = "white"),
    axis.title.x = element_text(hjust = 0, color = "#007095"),
    axis.text.x = element_text(face = 'bold', colour = '#5bc0de', size = 12, angle = 75, vjust = .5),
    axis.title.y = element_text(hjust = 0, color = "#007095"),
    axis.text.y = element_text(face = 'bold', colour = '#5bc0de', size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "#dee2e6", colour = "white")
  )

print(grafico_varImp)

# Perfil Parcial
treinoMtcars_x <- dplyr::select(treinoMtcars, -mpg)
testeMtcars_x <- dplyr::select(testeMtcars, -mpg)

explainer_rf <- DALEX::explain(model = mtcars_modelos$cforest, data = testeMtcars_x, y = testeMtcars$mpg, label = "Random Forest")

pdp_rf_cyl <- model_profile(explainer = explainer_rf, variables = "cyl")

# Gráfico de PDP
grafico_pdp <- plot(pdp_rf_cyl, geom = "profiles") +
  labs(
    title = "Perfil de Dependência Parcial para cyl",
    x = "cyl",
    y = "mpg"
  )

print(grafico_pdp)
