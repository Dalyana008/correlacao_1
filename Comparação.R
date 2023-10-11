## Associacao##
###Comparação###
# Carregando pacotes
pacman::p_load(corrplot, ccaPP, lsa, minerva, Rfast)

# Função multi.cor para várias associações#
multi.cor <- function(x, y) {
  corr = cor(x, y) # Correlação
  corrD = dcor(x, y) # Distance correlation
  cos = cosine(x, y) # Distância do Cosseno 
  maxCor = maxCorProj(x, y) # Maximal correlation
  MIC = mine(x, y) #  Maximal Information Coefficient
  associacoes = as.data.frame(list(corr, corrD[4], cos, maxCor[1], MIC[1]))
  names(associacoes) = c('Correlação', 'Distãncia', 'Cosseno', 'Máxima', 'MIC')
  return(associacoes)
}

# Base de dados
#---------------------------------------------------
USArests1 <- USArrests
usarests_cor<- cor(USArests1) # Tabela de correlações
corrplot(usarests_cor, method = "square", order = 'AOE')
# De acordo com o correlograma há uma forte associação positiva entre murder e assault. Temos x= Assault e y = Murder

# Exemplo 1: Linear
#---------------------------------------------------
x <- USArests1$Assault
y <- USArests1$Murder

plot(x, y) # Plotar o gráfico

corList <- multi.cor(x, y)
corList

# Exemplo 1.1: Linear + ruído 1
#---------------------------------------------------
y1 <- y - runif(length(y), 0, 1)

plot(x, y1)

corList1 <- multi.cor(x, y1)
corList1

### Exemplo 1.2: Linear + ruído 2
#---------------------------------------------------
y2 <- y - runif(length(y), 0, 2)

plot(x, y2)

corList2 <- multi.cor(x, y2)
corList2

# Exemplo 2: Quadrática
#---------------------------------------------------
k <- x
l <- 5 - 1.7*x + x^2

plot(k, l)

corList <- multi.cor(k, l)
corList

# Exemplo 2.1: Quadrática + ruído 1
#---------------------------------------------------
l1 <- l - runif(length(l), -1, 1)

plot(k, l1)

corList3 <- multi.cor(k, l1)
corList3

# Exemplo 2.1: Quadrática + ruído 2
#---------------------------------------------------
l2 <- l - runif(length(l), -2, 2)

plot(k, l2)

corList4 <- multi.cor(k, l2)
corList4

