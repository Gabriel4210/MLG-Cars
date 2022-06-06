base <- read.csv('https://raw.githubusercontent.com/Gabriel4210/MLG-Cars/main/CarPrice_Assignment.csv')

#Pacotes
library(dplyr)
library(ggplot2)
library(DescTools)
library(corrplot)

base %>% head
base %>% summary()

#Tratando as variaveis
base[,2] = as.factor(base[,2])
base[,3] = as.factor(base[,3])
base[,4] = as.factor(base[,4])
base[,5] = as.factor(base[,5])
base[,6] = as.factor(base[,6])
base[,7] = as.factor(base[,7])
base[,8] = as.factor(base[,8])
base[,9] = as.factor(base[,9])
base[,15] = as.factor(base[,15])
base[,16] = as.factor(base[,16])
base[,18] = as.factor(base[,18])

#Fast EDA
  
  #Checando a correlacao entre as variaveis categoricas
    corrplot::corrplot(DescTools::PairApply(base[,c(2,3,4,5,6,7,8,9,15,16,18)], DescTools::CramerV), method = "number", type = "lower", tl.cex = 0.8)
    DescTools::PairApply(base[,c(2,3,4,5,6,7,8,9,15,16,18)], DescTools::CramerV)
  #Checando a correlacao entre as variaveis continuas
    corrplot(cor(base[,c(10,11,12,13,14,17,19,20,21,22,23,24,25,26)]), method = "number", type = "lower", tl.cex = 0.8)

    #Verificar a relação das variaveis com a de interesse
    plot(base$symboling,base$price)
    plot(base$CarName,base$price)
    plot(base$fueltype,base$price)
    plot(base$aspiration,base$price)
    plot(base$doornumber,base$price)
    plot(base$carbody,base$price)          #Parece não significante
    plot(base$drivewheel,base$price)
    plot(base$enginelocation,base$price)
    plot(base$enginetype,base$price)
    plot(base$cylindernumber,base$price)
    plot(base$fuelsystem,base$price)
    
    plot(base$wheelbase,base$price)
    plot(base$carlength,base$price)
    plot(base$carwidth,base$price)
    plot(base$carheight,base$price)        #Correlacao baixa
    plot(base$curbweight,base$price)
    plot(base$enginesize,base$price)
    plot(base$boreratio,base$price)
    plot(base$stroke,base$price)           #Correlacao baixa
    plot(base$compressionratio,base$price) #Correlacao baixa
    plot(base$horsepower,base$price)
    plot(base$peakrpm,base$price)          #Correlacao baixa
    plot(base$citympg,base$price)
    plot(base$highwaympg,base$price)
    
#Começando a modelagem
    fitmax = glm(price ~ . , family = gaussian(link = "identity"), data = base)
    fitmin = glm(price ~ 1 , family = gaussian(link = "identity"), data = base)
    fit1   = step(fitmax, direction = "backward", scope = formula(fitmin), trace = 0)
    fit2   = step(fitmin, direction = "both", scope = formula(fitmax), trace = 0)
    
    fit1$anova
    fit1$coefficients
    
    fit2$anova
    fit2$coefficients
    
    summary(fitmax)
    summary(fitmin)
    
    summary(fit1)
    #A variavel carname utiliza muitos graus de liberdade e só parece ser
    #significativa para alguns modelos especificos como BMW, Peugeot, Subaru e Toyouta
    #Então criarei variáveis dummy para esses modelos e tirarei CarName do modelo para 
    #Ver como fica o novo ajuste
    
#Dummys 
    table(base$CarName)
    base$BMW = ifelse(base$CarName == "bmw 320i" | base$CarName == "bmw x1" |
                        base$CarName == "bmw x3" | base$CarName == "bmw x4" |
                        base$CarName == "bmw x5" | base$CarName == "bmw z4", 1, 0)
    
    base$Peugeot = ifelse(base$CarName == "peugeot 304" | base$CarName == "peugeot 504" |
                            base$CarName == "peugeot 504 (sw)" | base$CarName == "peugeot 505s turbo diesel" |
                            base$CarName == "peugeot 604sl", 1, 0)
    
    base$Toyota = ifelse(base$CarName == "toyota carina" | base$CarName == "toyota celica gt" |
                        base$CarName == "toyota celica gt liftback" | base$CarName == "toyota corolla" |
                        base$CarName == "toyota corolla 1200" | base$CarName == "toyota corolla 1600 (sw)" |
                        base$CarName == "toyota corolla liftback" | base$CarName == "toyota corolla tercel" |
                        base$CarName == "toyota corona" | base$CarName == "toyota corona hardtop" |
                        base$CarName == "toyota corona liftback" | base$CarName == "toyota corona mark ii" |
                        base$CarName == "toyota cressida" | base$CarName == "toyota mark ii" |
                        base$CarName == "toyota starlet" | base$CarName == "toyota tercel" |
                        base$CarName == "toyouta tercel", 1, 0)
    
    base$Subaru = ifelse(base$CarName == "subaru" | base$CarName == "subaru baja" |
                        base$CarName == "subaru brz" | base$CarName == "subaru dl" |
                        base$CarName == "subaru r1" | base$CarName == "subaru r2" |
                        base$CarName == "subaru trezia" | base$CarName == "subaru tribeca" , 1, 0)
    
    plot(as.factor(base$BMW),base$price)
    plot(as.factor(base$Peugeot),base$price)
    plot(as.factor(base$Toyota),base$price)
    plot(as.factor(base$Subaru),base$price)
    
    #Modelo sem CarName
    fitmax2 = glm(price ~ . , family = gaussian(link = "identity"), data = base[,-c(3)])
    fitmin2 = glm(price ~ 1 , family = gaussian(link = "identity"), data = base[,-c(3)])
    fit3   = step(fitmin2, direction = "both", scope = formula(fitmax2), trace = 0)
    
    fit3$anova
    summary(fit3)
    
    #Definido o modelo 3, vamos checar se os fatores de interação são significantes,
    # apenas nas variaveis que foram mais significativas no modelo, e nas qualitativa que 
    # apresentavam correlacao alta e rodar outro spepwise
    fit4=glm(formula = price ~ (enginesize + cylindernumber + enginetype + 
               horsepower + stroke + compressionratio + Toyota + BMW + enginelocation + 
               curbweight + carwidth + carlength + symboling + carbody + 
               boreratio + wheelbase)^2, family = gaussian(link = "identity"), 
             data = base)
    
    fitmax3 = glm(formula = price ~ enginesize + cylindernumber + enginetype + 
          horsepower + stroke + compressionratio + Toyota + BMW + enginelocation + 
          curbweight + carwidth + carlength + symboling + carbody + 
          boreratio + wheelbase + enginesize*cylindernumber + enginesize*enginetype + enginesize*horsepower + enginesize*stroke +
      enginesize*compressionratio + enginesize*Toyota + enginesize*BMW + enginesize*enginelocation + enginesize*curbweight + enginesize*carwidth +
      enginesize*carlength + enginesize*symboling + enginesize*carbody + enginesize*boreratio + enginesize*wheelbase, family = gaussian(link = "identity"), 
      data = base)
    fit4   = step(fitmin2, direction = "both", scope = formula(fit4), trace = 0)
    summary(fit4)
    # O modelo apenas atraves de métodos computacionais geram um modelo muito pouco parcimonioso
    # e não interpretavel, logo, para as variaveis continuas selecionarei as que apresentarem 
    # correlação maior que 0.7 em módulo para continuas e visualmente atraves dos boxplots para 
    # as categoricas
    
    fitpar = glm(price ~ carwidth + curbweight + enginesize + horsepower + enginelocation + 
                   BMW + enginesize*carwidth + enginesize*horsepower , family = gaussian(link = "identity"),data = base)
    summary(fitpar)

## ANALISE DE RESIDUOS
    par(mfrow=c(2,2))
    plot(fitpar)
    
    #Pontos Influntes
    i_n = influence(fitpar)$hat 
    plot(i_n)
    which.max(i_n)
    
    #Distância de Cook
    c_d = cooks.distance(fitpar)
    plot(c_d)
    which.max(c_d)    
    
    install.packages("faraway")
    library(faraway)
    halfnorm((c_d))
    halfnorm((i_n))
    
  #A observação 17 parece estar afetando o modelo então tentarei refazer sem ela para
  #visualizar a diferença
    fitpar2 = glm(price ~ carwidth + curbweight + enginesize + horsepower + enginelocation + 
                   BMW + enginesize*carwidth + enginesize*horsepower , family = gaussian(link = "identity"),data = base[-c(17),])
    summary(fitpar2)
    par(mfrow=c(2,2))
    plot(fitpar2)
    i_n2 = influence(fitpar2)$hat 
    plot(i_n2)
    which.max(i_n2)
    c_d2 = cooks.distance(fitpar2)
    plot(c_d2)
    which.max(c_d2)    
    halfnorm((c_d2))
    halfnorm((i_n2))
  #O Modelo apresentou melhoras
    
    residualPlots(fitpar2)
    dfbetasPlots(fitpar2)
    leveragePlots(fitpar2)
    
    install.packages("devtools")
    library(devtools)
    devtools::install_github("mkyou/glmTests")
    library(glmTests)
    res_envelope(fitpar2)
    res_vs_fitted(fitpar2)
    res_vs_index(fitpar2)
    influence(fitpar2)
    local_influence(fitpar2)
    
  #O modela ainda parece mal ajustado aos dados