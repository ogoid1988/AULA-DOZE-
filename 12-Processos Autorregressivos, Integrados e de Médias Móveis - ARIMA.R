                          #Aula 12 - Modelos  ARIMA

library("urca")                                #Carrega Pacote URCA
library(readxl)                                #Carrega Pacote readxl
library(pwt8)                                  #Carrega o pacote PWT8.0


data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoin�veis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (pa�s) igual a "Brazil", selecionando as colunas cujas vari�veis s�o "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (C�MBIO)

colnames(br) <-  c("PIB","Emprego","C�mbio")   #Renomeia as colunas para PIB, Trabalho e C�mbio

                                        #Separando as vari�veis
PIB <- br$PIB[45:62]                    #Cria o vetor para vari�vel PIB                  
EMPREGO <- br$Emprego[45:62]            #Cria o vetor para vari�vel EMPREGO
CAMBIO <- br$C�mbio[45:62]              #Cria o vetor para vari�vel CAMBIO
Anos <- seq(from=1994, to=2011, by=1)   #Cria um vetor para o tempo em anos de 1994 at� 2011 


#Analise para o Emprego------------------------------------------------------------------------------------

plot(EMPREGO, type = "l")                            #Cria gr�fico para o PIB
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como S�rie Temporal
plot(emprego, main="Pessoas Empregadas no Brasil", 
     ylab="Qte de Pessoas Empregadas-milh�es", 
     xlab="Ano")                                      #Cria gr�fico da S�rie Temporal

acf(emprego)                                          #Fun��o de Autocorrela��o
pacf(emprego)                                         ##Fun��o de Autocorrela��o Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regress�o linear simples do emprego em rela��o ao tempo
reglinEMP                                             #Exibe os resultados da regress�o linear
summary(reglinEMP)
plot(emprego)                                         #Gr�fcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regress�o linear estimada


#Removendo Tend�ncia

residuosEMP <- reglinEMP$residuals                    #Salva os res�duos no vetor residuosEMP
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regress�o linear dos res�duos em fun��o do tempo
plot(residuosEMP,type="l")                            #Gr�fico dos res�duos
abline(reglinEMPres, col="red")                      #Insere a linha de regress�o linear dos res�duos


#Removendo Tend�ncia por meio da diferen�a

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferen�a da s�rie de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da s�rie original coma diferen�a <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr�pafico somente para a serie da diferen�a

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferen�a
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferen�a
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

#Estimando a s�rie temporal

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
  
  
#CAMBIO MUDAR AS INFORMA��ES QUE EST�O SE REFERINDO AO EMPREGO
  
plot(EMPREGO, type = "l")                            #Cria gr�fico para o PIB
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como S�rie Temporal
plot(emprego, main="Pessoas Empregadas no Brasil", 
     ylab="Qte de Pessoas Empregadas-milh�es", 
     xlab="Ano")                                      #Cria gr�fico da S�rie Temporal

acf(emprego)                                          #Fun��o de Autocorrela��o
pacf(emprego)                                         ##Fun��o de Autocorrela��o Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regress�o linear simples do emprego em rela��o ao tempo
reglinEMP                                             #Exibe os resultados da regress�o linear
summary(reglinEMP)
plot(emprego)                                         #Gr�fcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regress�o linear estimada


#Removendo Tend�ncia

residuosEMP <- reglinEMP$residuals                    #Salva os res�duos no vetor residuosEMP
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regress�o linear dos res�duos em fun��o do tempo
plot(residuosEMP,type="l")                            #Gr�fico dos res�duos
abline(reglinEMPres, col="Blue")                      #Insere a linha de regress�o linear dos res�duos


#Removendo Tend�ncia por meio da diferen�a

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferen�a da s�rie de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da s�rie original coma diferen�a <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr�pafico somente para a serie da diferen�a

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferen�a
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferen�a
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

#Estimando a s�rie temporal

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




#Analise para o c�mbio--------------------------------------------------------------------------------------

plot(CAMBIO, type = "l")                            #Cria gr�fico para o CAMBIO
CAMBIO <- ts(CAMBIO, start = 1994, frequency = 1)  #Define como S�rie Temporal
plot(CAMBIO, main="C�mbio no Brasil", 
     ylab="Varia��o Cambial", 
     xlab="Ano")                                      #Cria gr�fico da S�rie Temporal

acf(CAMBIO)                                          #Fun��o de Autocorrela��o
pacf(CAMBIO)                                         ##Fun��o de Autocorrela��o Parcial
reglinCam <- lm(CAMBIO ~ Anos)                       #Regress�o linear simples do Cambio em rela��o ao tempo
reglinCam                                             #Exibe os resultados da regress�o linear
summary(reglinCam)
plot(CAMBIO)                                         #Gr�fcio dos dados
abline(reglinCam, col="red")                         #Insere a linha de regress�o linear estimada


#Removendo Tend�ncia

residuosCam <- reglinCam$residuals                    #Salva os res�duos no vetor residuosCAM
reglinCam <- lm(residuosCam ~ Anos)                #Regress�o linear dos res�duos em fun��o do tempo
plot(residuosCam,type="l")                            #Gr�fico dos res�duos
abline(reglinCam, col="red")                      #Insere a linha de regress�o linear dos res�duos


#Removendo Tend�ncia por meio da diferen�a

pdCambio <- diff(CAMBIO)                                #Calcula a primeira diferen�a da s�rie de dados
diferencaC <- (data.frame(CAMBIO[2:18],pdCambio))       #Exibe a tabela da s�rie original coma diferen�a <- 
diferencaC <- (data.frame(EMPREGO[2:18],pdCambio))

diferencaC <- ts(diferencaC, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(diferencaC, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdCambio, type="l")                                   #Cria gr�pafico somente para a serie da diferen�a

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdCambio1 <- diff(CAMBIO)                                            #Calculando-se a primeira diferen�a
TesteDF_Cambio1_trend <- ur.df(CAMBIO, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Cambio1_trend) 

pdcambio2 <- diff(diff(CAMBIO))                                      #Calculando-se a segunda diferen�a
TesteDF_cambio2_trend <- ur.df(pdcambio2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_cambio2_trend)

#Estimando a s�rie temporal

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



#Analise para o Pib-------------------------------------------------------------------------------------------------------

plot(PIB, type = "l")                            #Cria gr�fico para o CAMBIO
PIB <- ts(PIB, start = 1994, frequency = 1)  #Define como S�rie Temporal
plot(PIB, main="PIB Brasileiro", 
     ylab="Varia��o do PIB", 
     xlab="Ano")                                      #Cria gr�fico da S�rie Temporal

acf(PIB)                                          #Fun��o de Autocorrela��o
pacf(PIB)                                         ##Fun��o de Autocorrela��o Parcial
reglinPIB <- lm(PIB ~ Anos)                       #Regress�o linear simples do Cambio em rela��o ao tempo
reglinPIB                                             #Exibe os resultados da regress�o linear
summary(reglinPIB)
plot(PIB)                                         #Gr�fcio dos dados
abline(reglinPIB, col="red")                         #Insere a linha de regress�o linear estimada


#Removendo Tend�ncia

residuosPIB <- reglinPIB$residuals                    #Salva os res�duos no vetor residuosCAM
reglinPIB <- lm(residuosPIB ~ Anos)                #Regress�o linear dos res�duos em fun��o do tempo
plot(residuosPIB,type="l")                            #Gr�fico dos res�duos
abline(reglinPIB, col="red")                      #Insere a linha de regress�o linear dos res�duos


#Removendo Tend�ncia por meio da diferen�a

pdPIB1 <- diff(PIB)                                #Calcula a primeira diferen�a da s�rie de dados
diferencaP <- (data.frame(PIB[2:18],pdPIB))       #Exibe a tabela da s�rie original coma diferen�a <- 
diferencaP <- (data.frame(PIB[2:18],pdPIB))

diferencaP <- ts(diferencaP, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(diferencaP, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdPIB, type="l")                                   #Cria gr�pafico somente para a serie da diferen�a

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdPIB2 <- diff(PIB)                                            #Calculando-se a primeira diferen�a
TesteDF_PIB2_trend <- ur.df(PIB, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_PIB2_trend) 

pdPIB3 <- diff(diff(CAMBIO))                                      #Calculando-se a segunda diferen�a
TesteDF_PIB3_trend <- ur.df(pdPIB3, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_PIB3_trend)

#Estimando a s�rie temporal

#ARMA
arima121P <- arima(CAMBIO, c(1,2,1))
arima122P <- arima(CAMBIO, c(1,2,2))
arima123P <- arima(CAMBIO, c(1,2,3))

#MA
arima021P <- arima(CAMBIO, c(0,2,1))
arima022P <- arima(CAMBIO, c(0,2,2))
arima023P <- arima(CAMBIO, c(0,2,3))

#AR
arima120P <- arima(CAMBIO, c(1,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima120P,arima121P,
                   arima122P,arima123P,arima021P,arima022P,arima023P)

AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima120P","arima121P",
                "arima122P","arima123P","arima021P","arima022P","arima023P")) 
Resultados <- data.frame(Modelo,AIC,BIC)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
Resultados <- data.frame(Modelo,AIC,BIC)

View(Resultados)

#THE END





