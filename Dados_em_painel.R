#Exemplo livro Panel Data With R - Croissant & Millo (2019)

#Carrega os pacotes necessários
library(tidyverse)
library(plm)
library(AER)
library(lmtest)

#Carrega os dados
data(Grunfeld, package="AER")

#Plota a tabela
head(Grunfeld)
view(Grunfeld)

### Modelo Pooled ###
"""No modelo de dados empilhados o intercepto do modelo e seus coeficientes angulares são constantes ao longo
do tempo e no espaço, sendo que o termo de erro captura a diferença no tempo e entre os indivíduos. """

#Define a formula do modelo
model <- invest~value+capital

#Atribui a base dados ára um data frame
dados <- as.data.frame(Grunfeld)

#Analisa e atribui o modelo a um objeto
modelo_pooled <- plm(formula=model, data=dados, model="pooling", index = c("firm", "year"))

#Retorna as estatísticas do modelo analisado
summary(modelo_pooled)


### Modelo Fixed Effects ###
"""No modelo com efeitos fixos a regressão considera que os coeficientes angulares são constantes 
e o intercepto varia entre os indivíduos. """

#Analisa e atribui o modelo a um objeto
modelo_fixed <- plm(formula=model, data=dados, model="within", index = c("firm", "year"))

summary(modelo_fixed)

#Neste modelo cada empresa possui seu proprio intercepto
summary(fixef(modelo_fixed))



### Modelo Random ### 
"""No modelo com efeitos aleatórios a regressão considera que o intercepto assumem um valor médio
comum entre os indivíduos e os coeficientes angulares variam ao longo do tempo e também entre indivíduos """


#Analisa e atribui o modelo a um objeto
modelo_random <- plm(formula=model, data=dados, model="random", index = c("firm", "year"))

summary(modelo_random)

#No modelo random os efeitos individuais das firmas (“firms”) são considerados variáveis aleatórias


### Determinação da abordagem mais adequada ###
"""Para definir qual a abordagem mais se adequa ao modelo e aos dados é necessário aplicar um conjunto de testes."""

#Teste F (se o valor p é inferior a 0,05, o modelo de efeitos fixos é melhor do que o modelo empilhado)
pFtest(modelo_fixed,modelo_pooled)


#Teste Breusch-Pagan (se o p valor for inferior a 0,05 o modelo de efeitos aleatórios é preferível ao modelo empilhado)
plmtest(modelo_pooled, type="bp")

#Teste de Hausmann  (se o o valor p foi superior a 0,05 o modelo de efeitos aleatórios é preferível ao modelo de efeitos fixos)
phtest(modelo_fixed,modelo_random)



### Testes de aderência do modelo ###

#Teste Shapiro-Wilk de normalidade dos resíduos (se o valor de p for > que 0.05 os resíduos aderem a distribuição normal)
shapiro.test(modelo_random$residuals)

#Tesde de Breusch-Pagan para homocedasticidade  dos resíduos (se valor de p for > que 0.05 o modelo não apresenta heterocedasticidade)
bptest(modelo_random)

#Teste de Breusch-Godfrey para correlação serial (se valor de p for > que 0.05 o modelo não apresenta correlação serial)
bgtest(modelo_random)