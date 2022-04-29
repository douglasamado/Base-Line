library(dplyr)

H1 <- subset(H1eH2, pop == "H2")

sum(FBD_Cicla$total)
##### Modelo para linha-basica e análise conjunta (Para análise conjunta é melhor usar o modelo de ligação cloglog)

##### População 1 ############

# Caso o modelo não se ajuste aos dados você pode ir trocando a função de ligação (probit, logit, cloglog...)
glm.SUS<-glm(cbind(mortos, total-mortos)~log10(conc),family=binomial(link="probit"), 
              data =SUS_Cicla) #  subset(H1eH2, pop == "H1") Aqui você pode tentar também a distribuição quasibinomial.

summary(glm.SUS) # Summary do modelo, mostra se o modelo está ajustado aos dados (qui-quadrado e grau de liberdade)

qchisq(.95, df=5) # O grau de liberdade (df) é conforme os seus dados (o modelo mostra). O .95 é a probabilidade de estar fora do esperado

# Cálculo da Dose Letal 50 pelo pacote MASS
install.packages("MASS")
library(MASS) 

dose <- function(model, DL) {

        xp <- dose.p(model, p=DL) # Foram selecionadas as doses 10, 25 e 50. Mas pode ser adicionadas outras doses também  

        xp.ci <- xp + attr(xp, "SE") %*% matrix(qnorm(1 - 0.05/2)*c(-1,1), nrow=1)

        zp.est <- 10^(cbind(xp, attr(xp, "SE"), xp.ci[,1], xp.ci[,2]))

        dimnames(zp.est)[[2]] <- c("LD", "SE", "LCL","UCL")

        print(zp.est) # Mostra as doses selecionadas.
}

dose(glm.SUS, c(0.5, 0.9, 0.99))


# Função para fornecer o Slope e o Intercept
slope <- function(model) {
summary(model)$coefficients[,c(1,2)]
}

slope(glm.SUS)

# Calcular o p-valor do qui-quadrado

pchisq(7.11, 5)

# Heterogeneidade População Suscetível

het <- function(model){
  het <- model$deviance/model$df.residual
  print(paste('Heterogeneity Factor =', round(het,4)))
}

het(glm.SUS)

######### População 2 ################

glm.RES<-glm(cbind(mortos, total-mortos)~log10(conc),family=binomial(link="probit"),
             data = FBD_Cicla) # subset(H1eH2, pop == "H2") Aqui você pode tentar também a distribuição quasibinomial.

summary(glm.RES) # Summary do modelo, mostra se o modelo está ajustado aos dados (qui-quadrado e grau de liberdade)

# Quí-quadrado máximo tabelado

qchisq(.95, df=6) # O grau de liberdade (df) é conforme os seus dados (o modelo mostra). O .95 é a probabilidade de estar fora do esperado. Limite máximo aceitável do qui-quadrado.

# Calcular o p-valor do qui-quadrado

pchisq(2.60, 5)

# Cálculo da Dose Letal 50 pelo pacote MASS
library(MASS) 


dose <- function(model, DL) {
  library(MASS)
  xp <- dose.p(model, p=DL) # Foram selecionadas as doses 10, 25 e 50. Mas pode ser adicionadas outras doses também  
  
  xp.ci <- xp + attr(xp, "SE") %*% matrix(qnorm(1 - 0.05/2)*c(-1,1), nrow=1)
  
  zp.est <- 10^(cbind(xp, attr(xp, "SE"), xp.ci[,1], xp.ci[,2]))
  
  dimnames(zp.est)[[2]] <- c("LD", "SE", "LCL","UCL")
  
  print(zp.est) # Mostra as doses selecionadas.
}

dose(glm.RES, c(0.5, 0.9, 0.99))

# Função para fornecer o Slope e o Intercept
slope <- function(model) {
  summary(model)$coefficients[,c(1,2)]
}

slope(glm.RES)

# Calcular o p-valor do qui-quadrado

pchisq(10.68, 6)

# Heterogeneidade da População resistente 

het <- function(model){
  het <- model$deviance/model$df.residual
  print(paste('Heterogeneity Factor =', round(het,4)))
}

het(glm.RES)

###### LRT teste entre as populações ######################

# Modelo do LRT para verificar se as CL50 diferem entre si. (Melhor que usar intervalo de confiança, pois este último não tem precisão)
install.packages("ecotox")
library(ecotox)

test_LC50 <- ratio_test(model_1 = glm.SUS, model_2 = glm.RES, percentage = 50, log_x = T) # O percentage é a dose que você quer testar de cada modelo, neste caso é a CL50.
           
test_LC50



########## Teste de Paralelismo e igualdade ####################################
################################################################################
## O teste de Paralelismo e Igualdade testa a hipótese nula de que o slope das populações,
## são paralelos e iguais, desta forma valores de p-value maior que 0.05 aceitam a hipótese nula
## ou seja, o slope das populações testadas são iguais e paralelos. Valores de p-value menor que
## 0.05 rejeitam a hipótese nula, ou seja, os slopes das populaçãoes não são iguais e nem paralelos.

peq <- function(model1, model2){
  
  df1 <- model1$df.residual
  df2 <- model2$df.residual
  
  het <- model1$deviance/model1$df.residual
  
  #print(paste('Heterogeneity Factor =', round(het,4)))
  
  tstat <- (model2$deviance-model1$deviance)/(het*(df2-df1+1))
  testp <- 1-pf(tstat, df2-df1+1,df1)
  
  paste('test deviance =', round(tstat,4), 'df1=', df2-df1+1, 'df2=', df2,
        'Pvalue=', round(testp,3))
  
} 

peq(glm.RES, glm.SUS)  

