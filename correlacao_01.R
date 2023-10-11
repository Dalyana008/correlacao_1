#Machine Learning#
##Regras de associação##
###correlacaoo###

#Carregando o pacote USArrests (taxas de crimes nos EUA)

pacman::p_load(corrplot, dplyr, ggplot2)
data("USArrests")

# BASE DE DADOS USARRESTS SEM  UrbanPop #
Usarrests2<- USArrests%>% select(UrbanPop)
View(Usarrests2)
# TABELA DE CORRELAÇÃO COM TODAS AS VARIÁVEIS #
# TABELA DE CORRELAÇÃO COM TODAS AS VARIÁVEIS #
cor(USArrests)


# GRÁFICOS DE DISPERSÃO PAREADOS DAS VARIÁVEIS #
pairs(USArrests)
# CORROPLOT DAS VARIÁVEIS #
USArrestsCor <- cor(USArrests) # Tabela de correlações
corrplot(USArrestsCor, method = "number", order = 'alphabet')
corrplot(USArrestsCor, method = "number", order = 'alphabet')
corrplot(USArrestsCor, order = 'alphabet')
corrplot(USArrestsCor, method = "square", order = 'AOE')

