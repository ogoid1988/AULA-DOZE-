                          #Aula 12 - Modelos  ARIMA

library("urca")                                #Carrega Pacote URCA
library(readxl)                                #Carrega Pacote readxl
library(pwt8)                                  #Carrega o pacote PWT8.0


data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoiníveis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (país) igual a "Brazil", selecionando as colunas cujas variáveis são "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (CÂMBIO)

colnames(br) <-  c("PIB","Emprego","Câmbio")   #Renomeia as colunas para PIB, Trabalho e Câmbio

                                        #Separando as variáveis
PIB <- br$PIB[45:62]                    #Cria o vetor para variável PIB                  
EMPREGO <- br$Emprego[45:62]            #Cria o vetor para variável EMPREGO
CAMBIO <- br$Câmbio[45:62]              #Cria o vetor para variável CAMBIO
Anos <- seq(from=1994, to=2011, by=1)   #Cria um vetor para o tempo em anos de 1994 até 2011 


                                    #Analise para o Emprego

plot(EMPREGO, type = "l")                            #Cria gráfico para o PIB
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(emprego, main="Pessoas Empregadas no Brasil", 
     ylab="Qte de Pessoas Empregadas-milhões", 
     xlab="Ano")                                      #Cria gráfico da Série Temporal

acf(emprego)                                          #Função de Autocorrelação
pacf(emprego)                                         ##Função de Autocorrelação Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regressão linear simples do emprego em relação ao tempo
reglinEMP                                             #Exibe os resultados da regressão linear
summary(reglinEMP)
plot(emprego)                                         #Gráfcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regressão linear estimada


#Removendo Tendência

residuosEMP <- reglinEMP$residuals                    #Salva os resíduos no vetor residuosEMP
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosEMP,type="l")                            #Gráfico dos resíduos
abline(reglinEMPres, col="red")                      #Insere a linha de regressão linear dos resíduos


#Removendo Tendência por meio da diferença

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferença da série de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da série original coma diferença <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr´pafico somente para a serie da diferença

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferença
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferença
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

#Estimando a série temporal

arima123 <- arima(emprego, c(1,2,3))

#ARMA
arima120 <- arima(emprego, c(1,2,0))
arima121 <- arima(emprego, c(1,2,1))
arima122 <- arima(emprego, c(1,2,2))

arima220 <- arima(emprego, c(2,2,0))
arima221 <- arima(emprego, c(2,2,1))
arima222 <- arima(emprego, c(2,2,2))
arima223 <- arima(emprego, c(2,2,3))
#MA
arima021 <- arima(emprego, c(0,2,1))
arima022 <- arima(emprego, c(0,2,2))
arima023 <- arima(emprego, c(0,2,3))
#AR
arima020 <- arima(emprego, c(0,2,3))

#Escolher o melhor modelo com base no menor AIC/BIC
  estimacoes <- list(arima123,arima120,arima121,
                     arima122,arima220,arima221,
                     arima222,arima223,arima021,arima021,arima022,
                     arima023,arima120)
  AIC <- sapply(estimacoes, AIC)
  BIC <- sapply(estimacoes, BIC)
  Modelo <-c(list("arima123","arima120","arima121",
                  "arima122","arima220","arima221",
                  "arima222","arima223","arima021","arima021", "arima022",
                  "arima023","arima120")) 
  Resultados <- data.frame(Modelo,AIC,BIC)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
  Resultados <- data.frame(Modelo,AIC,BIC)

#CAMBIO MUDAR AS INFORMAÇÕES QUE ESTÃO SE REFERINDO AO EMPREGO
  
plot(EMPREGO, type = "l")                            #Cria gráfico para o PIB
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(emprego, main="Pessoas Empregadas no Brasil", 
     ylab="Qte de Pessoas Empregadas-milhões", 
     xlab="Ano")                                      #Cria gráfico da Série Temporal

acf(emprego)                                          #Função de Autocorrelação
pacf(emprego)                                         ##Função de Autocorrelação Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regressão linear simples do emprego em relação ao tempo
reglinEMP                                             #Exibe os resultados da regressão linear
summary(reglinEMP)
plot(emprego)                                         #Gráfcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regressão linear estimada


#Removendo Tendência

residuosEMP <- reglinEMP$residuals                    #Salva os resíduos no vetor residuosEMP
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosEMP,type="l")                            #Gráfico dos resíduos
abline(reglinEMPres, col="Blue")                      #Insere a linha de regressão linear dos resíduos


#Removendo Tendência por meio da diferença

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferença da série de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da série original coma diferença <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr´pafico somente para a serie da diferença

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferença
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferença
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

#Estimando a série temporal

arima123 <- arima(emprego, c(1,2,3))
  
#ARMA
arima120 <- arima(emprego, c(1,2,0))
arima121 <- arima(emprego, c(1,2,1))
arima122 <- arima(emprego, c(1,2,2))

arima220 <- arima(emprego, c(2,2,0))
arima221 <- arima(emprego, c(2,2,1))
arima222 <- arima(emprego, c(2,2,2))
arima223 <- arima(emprego, c(2,2,3))
#MA
arima021 <- arima(emprego, c(0,2,1))
arima022 <- arima(emprego, c(0,2,2))
arima023 <- arima(emprego, c(0,2,3))
#AR
arima020 <- arima(emprego, c(0,2,3))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,arima221,
                   arima222,arima223,arima021,arima021,arima022,
                   arima023,arima120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima120")) 
Resultados <- data.frame(Modelo,AIC,BIC)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
Resultados <- data.frame(Modelo,AIC,BIC)  


#Análise para o Câmbio
plot(CAMBIO, type = "l")                            #Cria gráfico para o CAMBIO
CAMBIO <- ts(CAMBIO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(CAMBIO, main="Câmbio no Brasil", 
     ylab="Variação Cambial", 
     xlab="Ano")                                      #Cria gráfico da Série Temporal

acf(CAMBIO)                                          #Função de Autocorrelação
pacf(CAMBIO)                                         ##Função de Autocorrelação Parcial
reglinCam <- lm(CAMBIO ~ Anos)                       #Regressão linear simples do Cambio em relação ao tempo
reglinCam                                             #Exibe os resultados da regressão linear
summary(reglinCam)
plot(CAMBIO)                                         #Gráfcio dos dados
abline(reglinCam, col="red")                         #Insere a linha de regressão linear estimada


#Removendo Tendência

residuosCam <- reglinCam$residuals                    #Salva os resíduos no vetor residuosCAM
reglinCam <- lm(residuosCam ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosCam,type="l")                            #Gráfico dos resíduos
abline(reglinCam, col="red")                      #Insere a linha de regressão linear dos resíduos


#Removendo Tendência por meio da diferença

pdCambio <- diff(CAMBIO)                                #Calcula a primeira diferença da série de dados
diferencaC <- (data.frame(CAMBIO[2:18],pdCambio))       #Exibe a tabela da série original coma diferença <- 
diferencaC <- (data.frame(EMPREGO[2:18],pdCambio))

diferencaC <- ts(diferencaC, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(diferencaC, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdCambio, type="l")                                   #Cria gr´pafico somente para a serie da diferença

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdCambio1 <- diff(CAMBIO)                                            #Calculando-se a primeira diferença
TesteDF_Cambio1_trend <- ur.df(CAMBIO, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Cambio1_trend) 

pdcambio2 <- diff(diff(CAMBIO))                                      #Calculando-se a segunda diferença
TesteDF_cambio2_trend <- ur.df(pdcambio2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_cambio2_trend)

#Estimando a série temporal

#ARMA
arima121c <- arima(CAMBIO, c(1,2,1))
arima122c <- arima(CAMBIO, c(1,2,2))

#MA
arima021c <- arima(CAMBIO, c(0,2,1))
arima022c <- arima(CAMBIO, c(0,2,2))

#AR
arima120c <- arima(CAMBIO, c(1,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima120c,arima121c,
                   arima122c,arima021c,arima022c)

AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima120c","arima121c",
                "arima122c","arima021c","arima022c")) 
Resultados <- data.frame(Modelo,AIC,BIC)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
Resultados <- data.frame(Modelo,AIC,BIC)


#Análise para o PIB

