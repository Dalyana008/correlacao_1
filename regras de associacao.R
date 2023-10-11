##### Regras de Associação 
pacman::p_load(
  # ETL
  data.table, dplyr,
  # MACHINE LEARNING
  caret,
  # REGRAS DE ASSOCIAÇÃO
  arules, arulesCBA, arulesViz,
  # TABELAS
  reactablefmtr
)
install.packages("data.table")
##### ETL #####
##carregando alguns pacotes##
install.packages("readxl")
library(readxl)
install.packages("readxl2")
library(readxl2)
# Instale e carregue o pacote 'dplyr'
install.packages("dplyr")
library(dplyr)



# Especifique o caminho para o arquivo Excel
caminho_arquivo_excel <- "C:/Users/daly_/Dropbox/R/bases_tratadas/Associacao_/consulta_cand_22_pe.xlsx"

# Leia o arquivo Excel
dados <- read_excel(caminho_arquivo_excel)


# filtrar apenas deputados estaduais e variáveis de perfil
federais_pe_2022 <- dados %>% filter(DS_CARGO == 'DEPUTADO FEDERAL') %>% select(TP_AGREMIACAO, NM_MUNICIPIO_NASCIMENTO, NR_IDADE_DATA_POSSE, DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO)

# observar se os dados estão com as classes certas
glimpse(federais_pe_2022)

# modificar nomes para facilitar análise e plot
names(federais_pe_2022) <- c(
  'agremiacao', 'municipio', 'idade', 'genero', 'instrucao', 'estado_civil', 'raca', 'ocupacao'
)

# discretizar variável numérica
federais_pe_2022[ , 3] <- discretizeDF(federais_pe_2022[ , 3]) # transforma variáveis numéricas em fatores

##### MINERAÇÃO #####
# mineração com a priori
regras_federais <- apriori(
  federais_pe_2022, 
  parameter = list(supp = 0.2, conf = 0.5, minlen = 2, maxlen = 5))

## limpar e organizar regras regras
# três casas decimais
quality(regras_federais) <- round(quality(regras_federais), digits = 3) 
# organizar por lift
regras_federais <- sort(regras_federais, by="lift") 
# remover regras redundantes
regras_federais_res <- regras_federais[!is.redundant(regras_federais, measure="lift")]

# inspecionar regras
inspect(regras_federais_res)

regras_federais_df = data.frame(
  lhs = labels(lhs(regras_federais_res)),
  rhs = labels(rhs(regras_federais_res)), 
  regras_federais_res@quality)

reactable(
  regras_federais_df, 
  defaultColDef = colDef(cell = data_bars(regras__df ,text_position = 'outside-base')),
  pagination = F
)

# gráfico de coordenadas
#instalando o pacote
install.packages("arulesViz")
install.packages("tidyr")

library(arulesViz)
plot(regras_federais_res, method="paracoord", control=list(reorder=T), measure=c("lift"), lty = "dotted")

# gráfico de relações agrupadas
plot(regras_federais_res, method="grouped", measure=c("lift"))

### APENAS BRANCOS ###

regras_brancos <- apriori(federais_pe_2022, control=list(verbose=F), parameter = list(minlen=2, supp=0.1, conf=0.3), appearance = list(lhs="raca=BRANCA", default="rhs"))

quality(regras_brancos)<-round(quality(regras_brancos),digits = 3)
regras_brancos<-sort(regras_brancos,by="lift")

regras_brancos_df = data.frame(
  lhs = labels(lhs(regras_brancos)),
  rhs = labels(rhs(regras_brancos)), 
  regras_brancos@quality)

reactable(regras_brancos_df, defaultColDef = colDef( cell = data_bars(regras_brancos_df,text_position = 'outside-base')
))

plot(regras_brancos,method="paracoord",control=list(reorder=T),measure=c("lift"),lty = "dotted")
